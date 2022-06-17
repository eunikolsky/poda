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
  describe "draftDuration" $
    context "when PR is merged" $ do
      it "returns zero when there are no events" $ do
        pull <- defaultDTI
        draftDuration pull `shouldBe` 0

      it "calculates duration from created to undraft" $ do
        undraftTime <- utcTime "2022-01-03T12:00:00Z"
        pull <- defaultDTI
        let pull' = pull { fEvents = [markReadyEvent undraftTime] }
        draftDuration pull' `shouldBe` 2.5 * nominalDay

      it "calculates duration from draft to undraft" $ do
        draftTime <- utcTime "2022-01-02T00:00:00Z"
        undraftTime <- utcTime "2022-01-04T12:00:00Z"
        pull <- defaultDTI
        let pull' = pull { fEvents = [markDraftEvent draftTime, markReadyEvent undraftTime] }
        draftDuration pull' `shouldBe` 2.5 * nominalDay

defaultDTI :: MonadFail m => m FreeDTI
defaultDTI = do
  fCreated <- utcTime "2022-01-01T00:00:00Z"
  fMerged <- Just <$> utcTime "2022-01-08T00:00:00Z"
  pure $ FreeDTI
    { fCreated
    , fMerged
    , fEvents = []
    }

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

data FreeDTI = FreeDTI
  { fCreated :: UTCTime
  , fMerged :: Maybe UTCTime
  , fEvents :: [PullEvent]
  }

instance DraftTimeInput FreeDTI where
  dtiCreated = fCreated
  dtiMerged = fMerged
  dtiEvents = fEvents

utcTime :: MonadFail m => String -> m UTCTime
utcTime = iso8601ParseM
