module Analyze where

import Data.Maybe
import Data.Time

import Database

-- | Defines the necessary data to calculate the draft duration.
class DraftTimeInput a where
  dtiCreated :: a -> UTCTime
  dtiMerged :: a -> Maybe UTCTime
  dtiEvents :: a -> [PullEvent]

-- | Calculates the draft duration of a PR.
-- When a PR is marked as draft or ready for review, an event is created;
-- we don't know the state of the PR at a point in time until we get an
-- event after that time.
draftDuration :: DraftTimeInput a => a -> NominalDiffTime
draftDuration dti = fromMaybe 0 $ diffUTCTime <$> maybeFirstMarkReady <*> pure draftStart
  where
    -- draft starts at the first "mark as draft" event or when the PR is created
    draftStart = fromMaybe (dtiCreated dti) maybeFirstMarkDraft
    maybeFirstMarkDraft = firstJust markDraftTime $ dtiEvents dti
    maybeFirstMarkReady = firstJust markReadyTime $ dtiEvents dti

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f
