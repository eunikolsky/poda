{-# LANGUAGE OverloadedStrings #-}

import Data.Functor.Identity
import Data.Time
import Data.Time.Format.ISO8601
import Test.Hspec

import Lib
import AnalyzeSpec

main :: IO ()
main = hspec $ do
  describe "parseRelLink" $ do
    it "returns the found link" $ do
      let input = "<anonymous_url>; rel=\"next\""
      parseRelLink "next" input `shouldBe` Just "anonymous_url"

    it "finds the link in a list" $ do
      let input = "<anonymous_url>; rel=\"prev\", <next_url>; rel=\"next\""
      parseRelLink "next" input `shouldBe` Just "next_url"

    it "returns Nothing when the link is not found" $
      parseRelLink "next" "<anonymous_url>; rel=\"prev\"" `shouldBe` Nothing

    it "returns Nothing when source string is empty" $
      parseRelLink "next" "" `shouldBe` Nothing

  describe "unfoldrM" $ do
    it "returns the first value" $ do
      let f = Identity . const ([0, 1], Nothing)
      runIdentity (unfoldrM f 0) `shouldBe` [0, 1]

    it "returns the combined value" $ do
      let f x = Identity ("ab", if x <= 1 then Just (x + 1) else Nothing)
      runIdentity (unfoldrM f 0) `shouldBe` "ababab"

    it "combines values to the right" $ do
      let f x = Identity ([x, x + 1], if x <= 3 then Just (x + 2) else Nothing)
      runIdentity (unfoldrM f 0) `shouldBe` [0, 1, 2, 3, 4, 5]

  describe "diffWorkTime" $ do
    it "calculates time difference within work week" $ do
      monday <- utcTime "2022-01-03T10:00:00Z"
      friday <- utcTime "2022-01-07T22:00:00Z"
      diffWorkTime' friday monday `shouldBe` mkNumberDays 4.5

    it "ignores full weekends" $ do
      friday <- utcTime "2022-01-07T12:00:00Z"
      nextMonday <- utcTime "2022-01-10T12:00:00Z"
      diffWorkTime' nextMonday friday `shouldBe` mkNumberDays 1

    it "ignores multiple full weekends" $ do
      friday <- utcTime "2021-12-31T12:00:00Z"
      laterMonday <- utcTime "2022-02-07T12:00:00Z"
      diffWorkTime' laterMonday friday `shouldBe` mkNumberDays 26

    it "calculates non-integer days diff" $ do
      thursday <- utcTime "2022-01-06T10:00:00Z"
      nextTuesday <- utcTime "2022-01-11T18:42:40Z"
      diffWorkTime' nextTuesday thursday `shouldBe`
        (NumberDays . WorkDiffTime $ 3 * nominalDay + (((8 * 60) + 42) * 60) + 40)

  AnalyzeSpec.spec

-- | @WorkDiffTime@ that has a @Show@ instance to display duration in days —
-- in order to have cleaner test failure messages.
newtype NumberDays = NumberDays WorkDiffTime
  deriving Eq

instance Show NumberDays where
  show (NumberDays x) = (show . realToFrac . (/ nominalDay) . unWorkDiffTime $ x) ++ " days"

mkNumberDays :: Real a => a -> NumberDays
mkNumberDays = NumberDays . WorkDiffTime . (* nominalDay) . realToFrac

diffWorkTime' :: UTCTime -> UTCTime -> NumberDays
diffWorkTime' from = NumberDays . diffWorkTime from
