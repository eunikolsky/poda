{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson hiding ((.=))
import Data.ByteString (ByteString)
import Data.Csv ((.=), DefaultOrdered, ToField, ToNamedRecord, header, headerOrder, namedRecord, toField, toNamedRecord)
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
import Text.Printf (printf)
import qualified Data.Bifunctor (second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Ord (Down(..))
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  CachedResponse
    url Text
    data ByteString
    nextLink Text Maybe
    eTag Text Maybe sql=etag
    lastModified UTCTime Maybe
    created UTCTime default=CURRENT_TIME
    UniqueURL url sql=unique_url
    deriving Show

  Pull
    number Int
    title Text
    url Text
    author Text
    created UTCTime
    merged UTCTime Maybe
    UniqueNumber number
    deriving Show
|]

-- | @Pull@ equality is based on their numbers only. Other fields are assumed to be
-- the same no matter which API call they came from.
instance Eq Pull where
  Pull { pullNumber = number0 } == Pull { pullNumber = number1 } = number0 == number1

instance FromJSON Pull where
  parseJSON = withObject "PR" $ \v -> do
    pullNumber <- v .: "number"
    pullTitle <- v .: "title"
    pullUrl <- v .: "pull_request" >>= (.: "html_url")
    pullAuthor <- v .: "user" >>= (.: "login")
    pullCreated <- v .: "created_at"
    pullMerged <- v .: "pull_request" >>= (.: "merged_at")
    pure $ Pull { pullNumber, pullTitle, pullUrl, pullAuthor, pullCreated, pullMerged }

data PullAnalysis = PullAnalysis
  { pullAnalysisPull :: Pull
  , pullAnalysisOpenTime :: Maybe (NominalDiffTime, WorkDiffTime)
  -- ^ Amount of time the PR was open (if merged). This is the time from open to merge time,
  -- regardless of any time it might have been closed in between.
  }
  deriving Show

instance ToField UTCTime where
  toField = C.pack . iso8601Show

-- | This allows to encode a @NominalDiffTime@ value into a CSV record.
instance ToField NominalDiffTime where
  toField = C.pack . show . truncate . realToFrac

instance ToNamedRecord PullAnalysis where
  toNamedRecord PullAnalysis { pullAnalysisPull = p, pullAnalysisOpenTime } = namedRecord
    [ "number" .= pullNumber p
    , "title" .= pullTitle p
    , "url" .= pullUrl p
    , "author" .= pullAuthor p
    , "created" .= pullCreated p
    , "merged" .= pullMerged p
    , "open_time" .= fmap fst pullAnalysisOpenTime
    , "work_open_time" .= fmap snd pullAnalysisOpenTime
    ]

instance DefaultOrdered PullAnalysis where
  headerOrder _ = header ["number", "title", "url", "author", "created", "merged", "open_time", "work_open_time"]

-- | Configuration information for the program.
data Config = Config
  { configToken :: String
    -- ^ GitHub token
  , configLocalTeam :: [Text]
    -- ^ Github names of our team
  , configRepo :: Repo
    -- ^ Repository to analyze
  , configLabel :: Text
    -- ^ Only PRs with this label are analyzed
  }
  deriving (Generic, Show)

instance FromJSON Config

-- | Contains `owner/repo`.
newtype Repo = Repo { unRepo :: Text }
  deriving Show

instance FromJSON Repo where
  parseJSON = fmap Repo . parseJSON

-- | List PRs in a repository when:
-- * they have the given @configLabel@;
-- * or they are created by a member of the @configLocalTeam@.
listPRs :: Config -> IO [Pull]
listPRs config = do
  manager <- newManager tlsManagerSettings

  labeledPRs <- listLabeledPRs manager
  authorsPRs <- traverse (`listAuthorPRs` manager) (configLocalTeam config)
  let prs = sortByNumberDesc . nub $ labeledPRs ++ concat authorsPRs

  runSqlite dbPath $ do
    -- delete all existing PRs first so that the uniqueness constraint doesn't block the insert;
    -- older PRs can't be lost because we always get the list of all PRs above
    deleteWhere ([] :: [Filter Pull])
    insertMany_ prs

  pure prs

  where
    listLabeledPRs :: Manager -> IO [Pull]
    listLabeledPRs = listPRs' $ "labels=" <> configLabel config

    listAuthorPRs :: Text -> Manager -> IO [Pull]
    listAuthorPRs author = listPRs' $ "creator=" <> author

    listPRs' :: Text -> Manager -> IO [Pull]
    listPRs' parameter manager = do
      let { link = GithubPath . mconcat $
        [ "https://api.github.com/repos/", unRepo . configRepo $ config
        , "/issues?per_page=100&state=all&", parameter
        ]
      }
      flip unfoldrM link $ \link -> do
        request <- githubRequest config link
        response <- cachedHTTPLbs request manager

        pure
          ( filter (fromOurTeam config) . fromMaybe [] . decode . httpRData $ response
          , GithubPath <$> httpRNextLink response
          )

    sortByNumberDesc = sortOn (Data.Ord.Down . pullNumber)

analyzePRs :: [Pull] -> [PullAnalysis]
analyzePRs = fmap analyze
  where
    analyze :: Pull -> PullAnalysis
    analyze pull@Pull { pullCreated, pullMerged } = PullAnalysis
      { pullAnalysisPull = pull
      , pullAnalysisOpenTime = openTime pullCreated pullMerged
      }

    openTime pullCreated maybePullMerged = do
      pullMerged <- maybePullMerged
      pure
        ( pullMerged `diffUTCTime` pullCreated
        , pullMerged `diffWorkTime` pullCreated
        )

printPRs :: [Pull] -> IO ()
printPRs prs = printAll >> printRemaining
  where
    printCount = 5
    printAll = mapM_ print $ take printCount prs
    printRemaining = putStrLn $ mconcat ["and ", show $ (length prs - printCount) `noLessThan` 0, " more…"]

noLessThan :: Ord a => a -> a -> a
noLessThan = max

fromOurTeam :: Config -> Pull -> Bool
fromOurTeam config Pull { pullAuthor } = pullAuthor `elem` configLocalTeam config

newtype GithubPath = GithubPath Text
  deriving Show

dbPath :: Text
dbPath = "cache.sqlite"

data HTTPResponse = HTTPResponse
  { httpRData :: BL.ByteString
  , httpRNextLink :: Maybe Text
  }

cachedHTTPLbs :: Request -> Manager -> IO HTTPResponse
cachedHTTPLbs req mgr = runSqlite dbPath $ do
  let url = T.pack . show . getUri $ req
  maybeCachedResponse <- selectFirst [CachedResponseUrl ==. url] []
  let reqWithCache = applyCachedResponse (entityVal <$> maybeCachedResponse) req
  -- liftIO . putStrLn $ mconcat ["URL ", T.unpack url, " etag: ", maybe "N/A" show (maybeCachedResponse >>= cachedResponseETag . entityVal)]

  resp <- liftIO $ httpLbs reqWithCache mgr

  let upsert = maybe insert_ (replace . entityKey) maybeCachedResponse
      eTag = lookup "ETag" $ responseHeaders resp
      lastModified = lookup "Last-Modified" $ responseHeaders resp
      hasETagOrLastModified = isJust eTag || isJust lastModified

  if responseStatus resp == notModified304
    then let cachedResponse = entityVal . fromJust $ maybeCachedResponse
      in pure $ HTTPResponse
        { httpRData = BL.fromStrict . cachedResponseData $ cachedResponse
        , httpRNextLink = cachedResponseNextLink cachedResponse
        }

    else do
      let nextLink = lookup "Link" (responseHeaders resp) >>= parseRelLink "next" . decodeUtf8

      when (statusIsSuccessful (responseStatus resp) && hasETagOrLastModified) $ do
        now <- liftIO getCurrentTime
        upsert $ CachedResponse
          { cachedResponseUrl = url
          , cachedResponseData = BL.toStrict $ responseBody resp
          , cachedResponseNextLink = nextLink
          , cachedResponseETag = decodeUtf8 <$> eTag
          , cachedResponseLastModified = lastModified >>= parseLastModified
          , cachedResponseCreated = now
          }

      pure $ HTTPResponse
        { httpRData = responseBody resp
        , httpRNextLink = nextLink
        }

applyCachedResponse :: Maybe CachedResponse -> Request -> Request
applyCachedResponse Nothing req = req
applyCachedResponse (Just cachedResponse) req = req
  { requestHeaders = requestHeaders req <> ifNoneMatch <> ifModifiedSince }
  where
    ifNoneMatch = maybe [] (pure . ("If-None-Match",) . encodeUtf8) (cachedResponseETag cachedResponse)
    ifModifiedSince = maybe [] (pure . ("If-Modified-Since",) . C.pack . formatTime defaultTimeLocale rfc2616DateFormat) (cachedResponseLastModified cachedResponse)

parseLastModified :: ByteString -> Maybe UTCTime
parseLastModified = parseTimeM False defaultTimeLocale rfc2616DateFormat . C.unpack

rfc2616DateFormat :: String
rfc2616DateFormat = "%a, %d %b %Y %T GMT"

migrateDB :: IO ()
migrateDB = runSqlite dbPath $ runMigration migrateAll

githubRequest :: Config -> GithubPath -> IO Request
githubRequest config (GithubPath githubPath) = do
  let token = configToken config
  request <- parseRequest . T.unpack $ githubPath
  pure $ request { requestHeaders =
    [ ("accept", "application/vnd.github.v3+json")
    , ("authorization", mconcat ["token ", C.pack token])
    , ("user-agent", "eunikolsky/poda")
    ] }

parseRelLink :: Text -> Text -> Maybe Text
parseRelLink rel text = findRel rel rels
  where
    findRel rel = safeHead . mapMaybe (isRel rel)
    isRel rel = (\[url, r] -> if r == "rel=\"" <> rel <> "\""
      then T.stripPrefix "<" =<< T.stripSuffix ">" url
      else Nothing) . T.splitOn "; "
    rels = filter (not . T.null) . T.splitOn ", " $ text

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

unfoldrM :: (Monoid a, Monad m) => (b -> m (a, Maybe b)) -> b -> m a
unfoldrM f = iter mempty
  where
    iter acc x = do
      (a, next) <- f x
      let newAcc = acc <> a
      maybe (pure newAcc) (iter newAcc) next

-- | Represents a time duration calculated between two time points skipping
-- weekend days between them.
newtype WorkDiffTime = WorkDiffTime { unWorkDiffTime :: NominalDiffTime }
  deriving Eq

instance Show WorkDiffTime where
  show (WorkDiffTime dt) = mconcat ["WorkDiffTime ", show dt]

instance ToField WorkDiffTime where
  toField = toField . unWorkDiffTime

diffWorkTime :: UTCTime -> UTCTime -> WorkDiffTime
diffWorkTime to from = WorkDiffTime $ fullDiff - weekends
  where
    fromDay = utctDay from
    toDay = utctDay to

    fullDiff = diffUTCTime to from
    weekends = (* nominalDay) . fromIntegral . length . filter isWeekend . fmap dayOfWeek $ [fromDay..toDay]

    isWeekend Saturday = True
    isWeekend Sunday = True
    isWeekend _ = False

avg :: Fractional a => [a] -> Maybe a
avg [] = Nothing
avg xs = Just (sum xs / genericLength xs)

newtype YearMonth = YearMonth (Integer, Int)
  deriving Eq

instance Show YearMonth where
  show (YearMonth (year, month)) = printf "%04d-%02d" year month

yearMonth :: Day -> YearMonth
yearMonth d = YearMonth (y, m)
  where (y, m, _) = toGregorian d

data AverageResult = AverageResult
  { arOpenDuration :: NominalDiffTime
  , arOpenWorkDuration :: WorkDiffTime
  }

data PRGroup = PRGroup
  { prgPRCount :: Int
  , prgMergedPRCount :: Int
  , prgAverageResult :: Maybe AverageResult
  }

averageWorkOpenTimeByMonth :: [PullAnalysis] -> [(YearMonth, PRGroup)]
averageWorkOpenTimeByMonth pulls = mapSecond avgTime . extendYearMonth $ groups
  where
    groups = groupBy ((==) `on` yearMonth . utctDay . pullCreated . pullAnalysisPull) pulls
    extendYearMonth = fmap (\xs@(PullAnalysis { pullAnalysisPull = Pull { pullCreated } } : _) -> (yearMonth $ utctDay pullCreated, xs))
    avgTime prs = PRGroup
      { prgPRCount = length prs
      , prgMergedPRCount = length $ filter (isJust . pullMerged . pullAnalysisPull) prs
      , prgAverageResult = let merged = mapMaybe pullAnalysisOpenTime prs in
        AverageResult
          <$> avg (map fst merged)
          <*> (WorkDiffTime <$> avg (map (unWorkDiffTime . snd) merged))
      }

formatDiffTime :: NominalDiffTime -> String
formatDiffTime = formatTime defaultTimeLocale "%ww %Dd %H:%M"

mapSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSecond f = map (Data.Bifunctor.second f)
