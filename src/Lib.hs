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
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  CachedResponse
    url Text
    data ByteString
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
    UniqueNumber number
    deriving Show
|]

instance FromJSON Pull where
  parseJSON = withObject "PR" $ \v -> do
    pullNumber <- v .: "number"
    pullTitle <- v .: "title"
    pullUrl <- v .: "url"
    pullAuthor <- v .: "user" >>= (.: "login")
    pullCreated <- v .: "created_at"
    pure $ Pull { pullNumber, pullTitle, pullUrl, pullAuthor, pullCreated }

-- | Configuration information for the program.
data Config = Config
  { configToken :: String
    -- ^ GitHub token
  , configLocalTeam :: [Text]
    -- ^ Github names of our team
  }
  deriving (Generic, Show)

instance FromJSON Config

-- | The current config. Technically it requires `IO`, but for now we expect to always have
-- a valid @config.json@ file — this is easier for faster development.
config :: Config
{-# NOINLINE config #-}
config = unsafePerformIO $ fromJust <$> decodeFileStrict' "config.json"

-- | Contains `owner/repo`.
newtype Repo = Repo String
  deriving Show

listPRs :: Repo -> IO ()
listPRs (Repo repo) = do
  manager <- newManager tlsManagerSettings
  request <- githubRequest . APIPath . mconcat $ ["/repos/", repo, "/pulls?per_page=50"]
  response <- cachedHTTPLbs request manager

  let prs :: [Pull] = filter fromOurTeam . fromMaybe [] . decode $ response
  mapM_ print prs

  runSqlite dbPath $ do
    alreadyAddedPulls <- selectList [] []
    let alreadyAddedNumbers = pullNumber . entityVal <$> alreadyAddedPulls
    insertMany_ $ filter (not . (`elem` alreadyAddedNumbers) . pullNumber) prs

fromOurTeam :: Pull -> Bool
fromOurTeam Pull { pullAuthor } = pullAuthor `elem` configLocalTeam config

newtype APIPath = APIPath String
  deriving Show

dbPath :: Text
dbPath = "cache.sqlite"

cachedHTTPLbs :: Request -> Manager -> IO BL.ByteString
cachedHTTPLbs req mgr = runSqlite dbPath $ do
  let url = T.pack . show . getUri $ req
  maybeCachedResponse <- selectFirst [CachedResponseUrl ==. url] []
  let reqWithCache = applyCachedResponse (entityVal <$> maybeCachedResponse) req

  resp <- liftIO $ httpLbs reqWithCache mgr

  let upsert = maybe insert_ (replace . entityKey) maybeCachedResponse
      eTag = lookup "ETag" $ responseHeaders resp
      lastModified = lookup "Last-Modified" $ responseHeaders resp
      hasETagOrLastModified = isJust eTag || isJust lastModified

  if responseStatus resp == notModified304
    then pure . BL.fromStrict . cachedResponseData . entityVal . fromJust $ maybeCachedResponse
    else do
      when (statusIsSuccessful (responseStatus resp) && hasETagOrLastModified) $ do
        now <- liftIO getCurrentTime
        upsert $ CachedResponse
          { cachedResponseUrl = url
          , cachedResponseData = BL.toStrict $ responseBody resp
          , cachedResponseETag = decodeUtf8 <$> eTag
          , cachedResponseLastModified = lastModified >>= parseLastModified
          , cachedResponseCreated = now
          }

      pure $ responseBody resp

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

githubRequest :: APIPath -> IO Request
githubRequest (APIPath apiPath) = do
  let token = configToken config
  request <- parseRequest $ mconcat ["https://api.github.com", apiPath]
  pure $ request { requestHeaders =
    [ ("accept", "application/vnd.github.v3+json")
    , ("authorization", mconcat ["token ", C.pack token])
    , ("user-agent", "eunikolsky/poda")
    ] }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
