{-# LANGUAGE TypeApplications #-}

module WorkDiffTimeSpec where

import Data.Time
import Test.Hspec

import SpecCommon
import WorkDiffTime hiding (regular, work)
import qualified WorkDiffTime as WorkTime (work)

spec :: Spec
spec =
  describe "diffWorkTime" $ do
    it "calculates time difference within work week" $ do
      let monday = utcTime "2022-01-03T10:00:00Z"
          friday = utcTime "2022-01-07T22:00:00Z"
      diffWorkTime' friday monday `shouldBe` mkNumberDays @Float 4.5

    it "ignores full weekends" $ do
      let friday = utcTime "2022-01-07T12:00:00Z"
          nextMonday = utcTime "2022-01-10T12:00:00Z"
      diffWorkTime' nextMonday friday `shouldBe` mkNumberDays @Int 1

    it "ignores multiple full weekends" $ do
      let friday = utcTime "2021-12-31T12:00:00Z"
          laterMonday = utcTime "2022-02-07T12:00:00Z"
      diffWorkTime' laterMonday friday `shouldBe` mkNumberDays @Int 26

    it "calculates non-integer days diff" $ do
      let thursday = utcTime "2022-01-06T10:00:00Z"
          nextTuesday = utcTime "2022-01-11T18:42:40Z"
      diffWorkTime' nextTuesday thursday `shouldBe`
        NumberDays (3 * nominalDay + (((8 * 60) + 42) * 60) + 40)

    -- FIXME test starting/ending on a weekend

-- | @NominalDiffTime@ that has a @Show@ instance to display duration in days —
-- in order to have cleaner test failure messages.
newtype NumberDays = NumberDays NominalDiffTime
  deriving Eq

instance Show NumberDays where
  show (NumberDays x) = (show @Float . realToFrac . (/ nominalDay) $ x) ++ " days"

mkNumberDays :: Real a => a -> NumberDays
mkNumberDays = NumberDays . (* nominalDay) . realToFrac

diffWorkTime' :: UTCTime -> UTCTime -> NumberDays
diffWorkTime' from = NumberDays . WorkTime.work . diffWorkTime from
