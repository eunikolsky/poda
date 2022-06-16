{-# LANGUAGE NamedFieldPuns #-}

module AnalyzeSpec where

import Data.Time
import Data.Time.Format.ISO8601
import Test.Hspec

import Analyze
import Database

spec :: Spec
spec =
  describe "draftDuration" $
    context "when PR is merged" $
      it "returns zero when there are no events" $ do
        pull <- defaultDTI
        draftDuration pull `shouldBe` 0

defaultDTI :: MonadFail m => m FreeDTI
defaultDTI = do
  fCreated <- utcTime "2022-01-01T00:00:00Z"
  fMerged <- Just <$> utcTime "2022-01-08T00:00:00Z"
  pure $ FreeDTI
    { fCreated
    , fMerged
    , fEvents = []
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
