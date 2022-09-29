{-# OPTIONS_GHC -Wno-orphans -Werror=missing-fields -Werror=missing-methods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding ((.=))
import Data.Aeson.Types hiding ((.=))
import Data.ByteString (ByteString)
import Data.Csv ((.=), DefaultOrdered, ToField, ToNamedRecord, header, headerOrder, namedRecord, toField, toNamedRecord)
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.Persist.Sqlite hiding (upsert)
import GHC.Generics hiding (from, to)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
import System.IO (hPutStrLn, stderr)
import qualified Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as SQL

import Analyze
import Concurrency
import Database
import EventType
import WorkDiffTime hiding (regular, work)
import qualified WorkDiffTime as WorkTime (regular, work)

-- | A "raw" version of @PullEvent@ that can be decoded from JSON.
-- Decoding @PullEvent@ directly doesn't work because:
-- 1. @parseJSON@ doesn't know the PR that the events are associated with;
-- 2. we're only interested in a subset of event types, but I can't find a
--    way to parse only "good" events from an array in JSON.
data PullEventJSON = PullEventJSON
  { pejId :: Int
  , pejType :: Text
  , pejActor :: Text
  , pejCreated :: UTCTime
  }
  deriving (Show)

instance FromJSON PullEventJSON where
  parseJSON = withObject "PullEvent" $ \v -> PullEventJSON
    <$> v .: "id"
    <*> v .: "event"
    <*> (v .: "actor" >>= (.: "login"))
    <*> v .: "created_at"

pullEventFromJSON :: SQL.Key Pull -> PullEventJSON -> Maybe PullEvent
pullEventFromJSON pullId PullEventJSON {..} = do
  pullEventType <- mkEventType pejType
  pure $ PullEvent
    { pullEventGhId = pejId
    , pullEventType = pullEventType
    , pullEventActor = pejActor
    , pullEventCreated = pejCreated
    , pullEventPull = pullId
    }

parsePullEvents :: SQL.Key Pull -> Value -> Either String [PullEvent]
parsePullEvents pullId value = do
  eventJsons <- parseEither parseJSON value
  pure $ mapMaybe (pullEventFromJSON pullId) eventJsons

-- | A "raw" version of @PullEvent@ (from the `timeline` github API) that can
-- be directly decoded from JSON. See also `PullEventJSON`.
data PullTimelineEventJSON = PullTimelineEventJSON
  { ptejId :: !(Maybe Int)
  , ptejType :: !Text
  , ptejReviewState :: !(Maybe Text)
  , ptejActor :: !(Maybe Text)
  , ptejCreated :: !(Maybe UTCTime)
  }

instance FromJSON PullTimelineEventJSON where
  parseJSON = withObject "PullEvent" $ \v -> PullTimelineEventJSON
    <$> v .:? "id"
    <*> v .: "event"
    <*> v .:? "state"
    <*> (v .:? "user" >>= maybe (pure Nothing) (.: "login"))
    <*> v .:? "submitted_at"

pullEventFromTimelineJSON :: SQL.Key Pull -> PullTimelineEventJSON -> Maybe PullEvent
pullEventFromTimelineJSON pullEventPull PullTimelineEventJSON{..} = do
  guard $ ptejType == "reviewed"
  pullEventType <- mkEventType =<< ptejReviewState
  pullEventGhId <- ptejId
  pullEventActor <- ptejActor
  pullEventCreated <- ptejCreated
  pure PullEvent{..}

parsePullTimelineEvents :: SQL.Key Pull -> BL.ByteString -> Either String [PullEvent]
parsePullTimelineEvents pullId bs = do
  timelineEventJsons <- eitherDecode @[PullTimelineEventJSON] bs
  pure $ mapMaybe (pullEventFromTimelineJSON pullId) timelineEventJsons

data PullAnalysis = PullAnalysis
  { pullAnalysisPull :: Pull
  , pullAnalysisOpenTime :: Maybe WorkDiffTime
  -- ^ Amount of time the PR was open (if merged). This is the time from open to merge time,
  -- regardless of any time it might have been closed in between.
  , pullAnalysisDraftDuration :: Maybe WorkDiffTime
  -- ^ Total amount of time the PR was in draft.
  }
  deriving Show

-- TODO remove orphan instances
instance ToField UTCTime where
  toField = C.pack . iso8601Show

-- TODO remove orphan instances
-- | This allows to encode a @NominalDiffTime@ value into a CSV record.
instance ToField NominalDiffTime where
  toField = C.pack . show @Int . truncate @Double . realToFrac

instance ToNamedRecord PullAnalysis where
  toNamedRecord PullAnalysis { pullAnalysisPull = p, pullAnalysisOpenTime, pullAnalysisDraftDuration } = namedRecord
    [ "repo" .= pullRepo p
    , "number" .= pullNumber p
    , "title" .= pullTitle p
    , "url" .= pullUrl p
    , "author" .= pullAuthor p
    , "created" .= pullCreated p
    , "merged" .= pullMerged p
    , "open_time" .= fmap WorkTime.regular pullAnalysisOpenTime
    , "work_open_time" .= fmap WorkTime.work pullAnalysisOpenTime
    , "draft_time" .= fmap WorkTime.regular pullAnalysisDraftDuration
    , "work_draft_time" .= fmap WorkTime.work pullAnalysisDraftDuration
    ]

instance DefaultOrdered PullAnalysis where
  headerOrder _ = header
    [ "repo", "number", "title", "url", "author", "created", "merged"
    , "open_time", "work_open_time"
    , "draft_time", "work_draft_time"
    ]

-- | Configuration information for the program.
data Config = Config
  { configToken :: String
    -- ^ GitHub token
  , configLocalTeam :: [Text]
    -- ^ Github names of our team
  , configRepos :: [Repo]
    -- ^ Repositories to analyze
  , configFirstSprintStart :: Day
    -- ^ Start day of the first sprint
  }
  deriving (Generic, Show)

instance FromJSON Config

-- | Contains `owner/repo`.
newtype Repo = Repo { unRepo :: Text }
  deriving Show

instance FromJSON Repo where
  parseJSON = fmap Repo . parseJSON

maxConcurrentDownloads :: MaxResources
maxConcurrentDownloads = MaxResources 4

-- | List PRs in a repository when:
-- * they have the given @configLabel@;
-- * or they are created by a member of the @configLocalTeam@.
listPRs :: Config -> IO [MPull]
listPRs config = do
  manager <- newManager tlsManagerSettings

  logLock <- newMVar ()

  let authorRepos = [(author, repo) | author <- configLocalTeam config, repo <- configRepos config]
  authorsPRs <- forConcurrentlyN maxConcurrentDownloads authorRepos $ \(author, repo) ->
    listAuthorPRs logLock author repo manager
  let prs = sortByRepoAndNumberDesc . nub $ concat authorsPRs

  prKeys <- runSqlite dbPath $ do
    -- delete all existing PRs first so that the uniqueness constraint doesn't block the insert;
    -- older PRs can't be lost because we always get the list of all PRs above
    deleteWhere ([] :: [Filter Pull])
    insertMany prs

  mPulls <- downloadAllPREvents config manager $ zip prs prKeys

  runSqlite dbPath $ do
    deleteWhere ([] :: [Filter PullEvent])
    insertMany_ . concatMap mpEvents $ mPulls

  pure mPulls

  where
    listAuthorPRs :: MVar () -> Text -> Repo -> Manager -> IO [Pull]
    listAuthorPRs logLock author = listPRs' logLock $ "creator=" <> author

    listPRs' :: MVar () -> Text -> Repo -> Manager -> IO [Pull]
    listPRs' logLock parameter repo manager = do
      let { link = GithubPath . mconcat $
        [ "https://api.github.com/repos/", unRepo repo
        , "/issues?per_page=100&state=all&", parameter
        ]
      }
      flip unfoldrM link $ \nextLink -> do
        request <- githubRequest config nextLink
        logStr logLock $ show nextLink
        response <- cachedHTTPLbs request manager

        pure
          ( filter (fromOurTeam config) . fromMaybe [] . decode . httpRData $ response
          , GithubPath <$> httpRNextLink response
          )

sortByRepoAndNumberDesc :: [Pull] -> [Pull]
sortByRepoAndNumberDesc = sortOn (\Pull{..} -> (pullRepo, Data.Ord.Down pullNumber))

logStr :: MVar () -> String -> IO ()
logStr logLock = withMVar logLock . const . hPutStrLn stderr <=< addTime
  where
    addTime :: String -> IO String
    addTime s = (\t tz -> mconcat [show . snd . utcToLocalTimeOfDay tz . timeToTimeOfDay . utctDayTime $ t, " ", s])
      <$> getCurrentTime
      <*> getCurrentTimeZone

-- FIXME I hate how I have to pass Key separately because it's required by PullEvent;
-- on the other hand, Pull as is doesn't know its events…
-- TODO use Reader?
downloadAllPREvents :: Config -> Manager -> [(Pull, SQL.Key Pull)] -> IO [MPull]
downloadAllPREvents config manager pulls = do
  logLock <- newMVar ()

  let
    worker :: (Pull, SQL.Key Pull) -> IO MPull
    worker keyedPull@(pull@(Pull { pullEventsUrl }), _) = do
      logStr logLock $ show pullEventsUrl
      events <- downloadPREvents config manager keyedPull
      pure $ MPull pull events

  forConcurrentlyN maxConcurrentDownloads pulls worker

downloadPREvents :: Config -> Manager -> (Pull, SQL.Key Pull) -> IO [PullEvent]
downloadPREvents config manager (Pull { pullEventsUrl }, key) = do
  let link = GithubPath . mconcat $ [pullEventsUrl, "?per_page=100"]
  request <- githubRequest config link
  response <- cachedHTTPLbs request manager

  when (isJust $ httpRNextLink response) .
    error $ "downloadPREvents: TODO: implement pagination " <> show (httpRNextLink response)

  let events = eitherDecode (httpRData response) >>= parsePullEvents key
  case events of
    Right event -> pure event
    Left err -> error . mconcat $ ["downloadPREvents: parsing events from ", show link, " failed: ", show err]

-- | Merges two lists of `PullEvent`s and the output is ordered by increasing creation time.
mergePREvents :: [PullEvent] -> [PullEvent] -> [PullEvent]
mergePREvents events0 = sortBy (compare `on` pullEventCreated) . (++) events0

analyze :: MPull -> PullAnalysis
analyze mPull@MPull { mpPull = pull@(Pull { pullCreated, pullMerged }) } = PullAnalysis
  { pullAnalysisPull = pull
  , pullAnalysisOpenTime = diffWorkTime <$> pullMerged <*> pure pullCreated
  , pullAnalysisDraftDuration = draftDuration mPull
  }

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

data HTTPResponse = HTTPResponse
  { httpRData :: BL.ByteString
  , httpRNextLink :: Maybe Text
  }

cachedHTTPLbs :: Request -> Manager -> IO HTTPResponse
cachedHTTPLbs req mgr = do
  let url = T.pack . show . getUri $ req
      withConcurrentSqlite = runSqlite dbPath . retryOnBusy
  maybeCachedResponse <- withConcurrentSqlite $ selectFirst [CachedResponseUrl ==. url] []
  let reqWithCache = applyCachedResponse (entityVal <$> maybeCachedResponse) req
  -- liftIO . putStrLn $ mconcat ["URL ", T.unpack url, " etag: ", maybe "N/A" show (maybeCachedResponse >>= cachedResponseETag . entityVal)]

  resp <- liftIO $ httpLbs reqWithCache mgr

  let eTag = lookup "ETag" $ responseHeaders resp
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
        withConcurrentSqlite $ do
          let upsert = maybe insert_ (replace . entityKey) maybeCachedResponse
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
parseRelLink rel text = findRel rels
  where
    findRel = safeHead . mapMaybe isRel
    isRel = (\[url, r] -> if r == "rel=\"" <> rel <> "\""
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

avg :: Fractional a => [a] -> Maybe a
avg [] = Nothing
avg xs = Just (sum xs / genericLength xs)

data AverageResult = AverageResult
  { arOpenDuration :: NominalDiffTime
  , arOpenWorkDuration :: NominalDiffTime
  }

data PRGroup = PRGroup
  { prgPRCount :: Int
  , prgMergedPRCount :: Int
  , prgAverageResult :: Maybe AverageResult
  , prgAverageWorkDraftDuration :: Maybe NominalDiffTime
  }

averageWorkOpenTime :: [PullAnalysis] -> PRGroup
averageWorkOpenTime prs = PRGroup
  { prgPRCount = length prs
  , prgMergedPRCount = length $ filter (isJust . pullMerged . pullAnalysisPull) prs
  , prgAverageResult = let merged = mapMaybe pullAnalysisOpenTime prs in
    AverageResult
      <$> avg (map WorkTime.regular merged)
      <*> avg (map WorkTime.work merged)
  , prgAverageWorkDraftDuration =
      let draftDurations :: [NominalDiffTime] = mapMaybe (fmap WorkTime.work . pullAnalysisDraftDuration) prs
      in avg draftDurations
  }

formatDiffTime :: NominalDiffTime -> String
formatDiffTime = formatTime defaultTimeLocale "%ww %Dd %H:%M"

mapSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSecond f = map (Data.Bifunctor.second f)

newtype Sprint = Sprint Day -- ^ Start day of a 2-week's long sprint.

instance Show Sprint where
  show (Sprint d) = mconcat ["[", show d, "…", show $ addDays 13 d, "]"]

nextSprint :: Sprint -> Sprint
nextSprint (Sprint d) = Sprint $ addDays 14 d

inSprint :: Sprint -> Day -> Bool
inSprint (Sprint d) day = day >= d && day <= addDays 13 d

sprintFilename :: Sprint -> String
sprintFilename (Sprint d) = mconcat [show d, "-", show $ addDays 13 d]

groupBySprint :: Sprint -> [PullAnalysis] -> [(Sprint, [PullAnalysis])]
groupBySprint firstSprint prs = (\s -> (s, filter (inSprint s . prDay) prs)) <$> allSprints
  where
    newestPR = prDay . maximumBy (compare `on` pullCreated . pullAnalysisPull) $ prs
    allSprints = takeWhile (\(Sprint d) -> d < newestPR) $ iterate nextSprint firstSprint
    prDay = utctDay . pullCreated . pullAnalysisPull
