{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
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
|]

githubToken :: IO String
githubToken = getEnv "GH_TOKEN"

-- | Contains `owner/repo`.
newtype Repo = Repo String
  deriving Show

data PR = PR
  { id :: Int
  , title :: Text
  , url :: Text
  }
  deriving (Generic, Show)

instance FromJSON PR

listPRs :: Repo -> IO ()
listPRs (Repo repo) = do
  manager <- newManager tlsManagerSettings
  request <- githubRequest . APIPath . mconcat $ ["/repos/", repo, "/pulls?per_page=5"]
  response <- cachedHTTPLbs request manager

  let prs :: [PR] = fromMaybe [] . decode $ responseBody response
  mapM_ print prs

  putStrLn ""
  print $ responseStatus response
  forM_ (responseHeaders response) $ \(name, value) ->
    putStrLn $ mconcat [show name, ": ", C.unpack value]

newtype APIPath = APIPath String
  deriving Show

dbPath :: Text
dbPath = "cache.sqlite"

cachedHTTPLbs :: Request -> Manager -> IO (Response BL.ByteString)
cachedHTTPLbs req mgr = runSqlite dbPath $ do
  let url = T.pack . show . getUri $ req
  maybeCachedResponse <- selectFirst [CachedResponseUrl ==. url] []
  let reqWithCache = applyCachedResponse (entityVal <$> maybeCachedResponse) req

  resp <- liftIO $ httpLbs reqWithCache mgr
  now <- liftIO getCurrentTime
  let upsert = maybe insert_ (replace . entityKey) maybeCachedResponse
  when (statusIsSuccessful $ responseStatus resp) . upsert
    $ CachedResponse
      { cachedResponseUrl = url
      , cachedResponseData = BL.toStrict $ responseBody resp
      , cachedResponseETag = decodeUtf8 <$> lookup "ETag" (responseHeaders resp)
      , cachedResponseLastModified = lookup "Last-Modified" (responseHeaders resp) >>= parseLastModified
      , cachedResponseCreated = now
      }
  return resp

applyCachedResponse :: Maybe CachedResponse -> Request -> Request
applyCachedResponse Nothing req = req
applyCachedResponse (Just cachedResponse) req = req
  { requestHeaders = requestHeaders req ++
    [ ("If-None-Match", maybe "" encodeUtf8 (cachedResponseETag cachedResponse))
    , ("If-Modified-Since", maybe "" (C.pack . formatTime defaultTimeLocale rfc2616DateFormat) (cachedResponseLastModified cachedResponse))
    ]
  }

parseLastModified :: ByteString -> Maybe UTCTime
parseLastModified = parseTimeM False defaultTimeLocale rfc2616DateFormat . C.unpack

rfc2616DateFormat :: String
rfc2616DateFormat = "%a, %d %b %Y %T GMT"

migrateDB :: IO ()
migrateDB = runSqlite dbPath $ runMigration migrateAll

githubRequest :: APIPath -> IO Request
githubRequest (APIPath apiPath) = do
  token <- githubToken
  request <- parseRequest $ mconcat ["https://api.github.com", apiPath]
  pure $ request { requestHeaders =
    [ ("accept", "application/vnd.github.v3+json")
    , ("authorization", mconcat ["token ", C.pack token])
    , ("user-agent", "eunikolsky/poda")
    ] }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
