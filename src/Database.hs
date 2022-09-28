{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Control.Monad
import Data.Aeson
import Data.Function (on)
import Data.Time
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T

import EventType

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
    repo Text
    number Int
    title Text
    url Text
    author Text
    isDraft Bool
    created UTCTime
    merged UTCTime Maybe
    eventsUrl Text
    UniqueNumber number
    deriving Show

  PullEvent
    ghId Int
    type EventType
    actor Text
    created UTCTime
    pull PullId OnDeleteCascade
    UniqueGhId ghId
    deriving Eq Show
|]

-- | @Pull@ equality is based on `repo + number` only. Other fields are assumed to be
-- the same no matter which API call they came from.
instance Eq Pull where
  (==) = (==) `on` (\Pull{..} -> (pullRepo, pullNumber))

instance FromJSON Pull where
  parseJSON = withObject "PR" $ \v -> do
    pullNumber <- v .: "number"
    pullTitle <- v .: "title"
    pullUrl <- v .: "pull_request" >>= (.: "html_url")
    pullAuthor <- v .: "user" >>= (.: "login")
    pullIsDraft <- v .: "draft"
    pullCreated <- v .: "created_at"
    pullMerged <- v .: "pull_request" >>= (.: "merged_at")
    pullEventsUrl <- v .: "events_url"

    pullRepo <- maybe
      (fail $ "Couldn't get repo from URL " <> T.unpack pullUrl)
      pure
      $ extractRepoFromURL pullUrl

    pure $ Pull {..}

-- TODO Maybe it's not the best way to get the repo text, but there is no direct way to inject
-- it into `parseJSON`. The caller has this information, so it could pass it here somehow.
extractRepoFromURL :: Text -> Maybe Text
extractRepoFromURL url = do
  repoWithStuff <- T.stripPrefix "https://github.com/" url
  let (owner : repo : _) = T.splitOn "/" repoWithStuff
  pure $ mconcat [owner, "/", repo]

markReadyTime :: PullEvent -> Maybe UTCTime
markReadyTime PullEvent { pullEventType = MarkReady, pullEventCreated } = Just pullEventCreated
markReadyTime _ = Nothing

markDraftTime :: PullEvent -> Maybe UTCTime
markDraftTime PullEvent { pullEventType = MarkDraft, pullEventCreated } = Just pullEventCreated
markDraftTime _ = Nothing

dbPath :: Text
dbPath = "cache.sqlite"

migrateDB :: IO ()
migrateDB = runSqlite dbPath $ runMigration migrateAll

-- | Drops the tables that can be derived from the network responses, that is everything except
-- `CachedResponse` (they are derived because if you still have the filled in `CachedResponse`,
-- it's faster to parse the PR information from the cache (and not increase the request count
-- against the rate limit) than to download everything from scratch). If you need to make an
-- incompatible change in that one and don't provide a migration, you can just @rm -f cache.sqlite@.
dropDerivedTables :: IO ()
dropDerivedTables = runSqlite dbPath $
  -- FIXME how to get table names from persistent?
  forM_ ["pull_event", "pull"] $ flip rawExecute [] . ("DROP TABLE " <>)
