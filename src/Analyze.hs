module Analyze where

import Data.Time

import Database

-- | Defines the necessary data to calculate the draft duration.
class DraftTimeInput a where
  dtiCreated :: a -> UTCTime
  dtiMerged :: a -> Maybe UTCTime
  dtiEvents :: a -> [PullEvent]

draftDuration :: DraftTimeInput a => a -> NominalDiffTime
draftDuration = const 0
