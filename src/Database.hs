{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Control.Monad
import Data.Aeson
import Data.Time
import Data.ByteString (ByteString)
import Data.Ord (comparing)
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH

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
    number Int
    title Text
    url Text
    author Text
    eventsUrl Text
    created UTCTime
    merged UTCTime Maybe
    UniqueNumber number
    deriving Show

  PullEvent
    ghId Int
    type EventType
    created UTCTime
    pull PullId OnDeleteCascade
    UniqueGhId ghId
    deriving Show
|]

-- | @Pull@ equality is based on their numbers only. Other fields are assumed to be
-- the same no matter which API call they came from.
instance Eq Pull where
  Pull { pullNumber = number0 } == Pull { pullNumber = number1 } = number0 == number1

instance Ord Pull where
  compare = comparing pullNumber

instance FromJSON Pull where
  parseJSON = withObject "PR" $ \v -> do
    pullNumber <- v .: "number"
    pullTitle <- v .: "title"
    pullUrl <- v .: "pull_request" >>= (.: "html_url")
    pullAuthor <- v .: "user" >>= (.: "login")
    pullEventsUrl <- v .: "events_url"
    pullCreated <- v .: "created_at"
    pullMerged <- v .: "pull_request" >>= (.: "merged_at")
    pure $ Pull { pullNumber, pullTitle, pullUrl, pullAuthor, pullEventsUrl, pullCreated, pullMerged }

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
