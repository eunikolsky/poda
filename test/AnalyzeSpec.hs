{-# LANGUAGE NamedFieldPuns #-}

module AnalyzeSpec where

import Data.Time
import Data.Time.Format.ISO8601
import Database.Persist.Sqlite
import Test.Hspec

import Analyze
import Database
import EventType

spec :: Spec
spec =
  describe "draftDuration" $ do
    context "when PR is merged" $ do
      it "returns zero when there are no events" $ do
        pull <- defaultDDI
        draftDuration pull `shouldBe` 0

      it "calculates duration from created to undraft" $ do
        undraftTime <- utcTime "2022-01-03T12:00:00Z"
        pull <- defaultDDI
        let pull' = pull { fEvents = [markReadyEvent undraftTime] }
        draftDuration pull' `shouldBe` 2.5 * nominalDay

      it "calculates duration from draft to undraft" $ do
        draftTime <- utcTime "2022-01-02T00:00:00Z"
        undraftTime <- utcTime "2022-01-04T12:00:00Z"
        pull <- defaultDDI
        let pull' = pull { fEvents = [markDraftEvent draftTime, markReadyEvent undraftTime] }
        draftDuration pull' `shouldBe` 2.5 * nominalDay

      it "sums up multiple duration periods starting with draft event" $ do
        times <- traverse utcTime -- pairs of draft-undraft times
          [ "2022-01-02T00:00:00Z"
          , "2022-01-02T06:00:00Z" -- 0.25 days
          , "2022-01-02T18:00:00Z"
          , "2022-01-04T12:00:00Z" -- 1.75 days
          , "2022-01-06T00:00:00Z"
          , "2022-01-07T00:00:00Z" -- 1 day
          ]
        let events = zipWith ($) (cycle [markDraftEvent, markReadyEvent]) times
        pull <- defaultDDI
        let pull' = pull { fEvents = events }
        draftDuration pull' `shouldBe` (0.25 + 1.75 + 1) * nominalDay

      it "sums up multiple duration periods starting with undraft event" $ do
        times <- traverse utcTime -- pairs of undraft-draft times
          [ "2022-01-02T00:00:00Z" -- 1 day
          , "2022-01-02T06:00:00Z"
          , "2022-01-02T18:00:00Z" -- 0.5 days
          , "2022-01-04T12:00:00Z"
          , "2022-01-06T00:00:00Z" -- 1.5 days
          ]
        let events = zipWith ($) (cycle [markReadyEvent, markDraftEvent]) times
        pull <- defaultDDI
        let pull' = pull { fEvents = events }
        draftDuration pull' `shouldBe` (1 + 0.5 + 1.5) * nominalDay

      it "calculates duration from draft to merged" $ do
        -- even though it doesn't make much sense according to github UI
        draftTime <- utcTime "2022-01-06T00:00:00Z"
        pull <- defaultDDI
        let pull' = pull { fEvents = [markDraftEvent draftTime] }
        draftDuration pull' `shouldBe` 2 * nominalDay

    context "when PR is not merged" $ do
      it "returns zero when there are no events" $ do
        pull <- defaultUnmergedDDI
        draftDuration pull `shouldBe` 0

      it "excludes duration from draft to unmerged" $ do
        times <- traverse utcTime -- pairs of undraft-draft times
          [ "2022-01-02T00:00:00Z" -- 1 day
          , "2022-01-02T06:00:00Z"
          , "2022-01-02T18:00:00Z" -- 0.5 days
          , "2022-01-04T12:00:00Z"
          ]
        let events = zipWith ($) (cycle [markReadyEvent, markDraftEvent]) times
        pull <- defaultUnmergedDDI
        let pull' = pull { fEvents = events }
        draftDuration pull' `shouldBe` (1 + 0.5) * nominalDay

defaultDDI :: MonadFail m => m FreeDDI
defaultDDI = do
  fCreated <- utcTime "2022-01-01T00:00:00Z"
  fMerged <- Just <$> utcTime "2022-01-08T00:00:00Z"
  pure $ FreeDDI
    { fCreated
    , fMerged
    , fEvents = []
    }

defaultUnmergedDDI :: MonadFail m => m FreeDDI
defaultUnmergedDDI = do
  x <- defaultDDI
  pure $ x { fMerged = Nothing }

markDraftEvent :: UTCTime -> PullEvent
markDraftEvent time = PullEvent
  { pullEventGhId = 0
  , pullEventType = MarkDraft
  , pullEventCreated = time
  , pullEventPull = toSqlKey 0
  }

markReadyEvent :: UTCTime -> PullEvent
markReadyEvent time = PullEvent
  { pullEventGhId = 0
  , pullEventType = MarkReady
  , pullEventCreated = time
  , pullEventPull = toSqlKey 0
  }

data FreeDDI = FreeDDI
  { fCreated :: UTCTime
  , fMerged :: Maybe UTCTime
  , fEvents :: [PullEvent]
  }

instance DraftDurationInput FreeDDI where
  ddiCreated = fCreated
  ddiMerged = fMerged
  ddiEvents = fEvents

utcTime :: MonadFail m => String -> m UTCTime
utcTime = iso8601ParseM
