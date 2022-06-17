module Analyze where

import Data.Maybe
import Data.Monoid
import Data.Time
import GHC.Stack (HasCallStack)

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
--
-- Assumptions:
-- * if a PR is merged, either the latest event before that is "mark as ready"
--   or there was never a "mark as draft".
-- * there can't be 2+ events of the same type in a row.
draftDuration :: DraftTimeInput a => a -> NominalDiffTime
draftDuration dti = maybe 0 getSum . mconcat . fmap (fmap Sum) $ draftDurations
  where
    eventPairs = chunksOf 2 (dtiEvents dti)
    draftDurations = map pairDuration eventPairs

    pairDuration :: HasCallStack => [PullEvent] -> Maybe NominalDiffTime
    pairDuration [draft, ready] = diffUTCTime <$> markReadyTime ready <*> markDraftTime draft
    pairDuration [ready] = diffUTCTime <$> markReadyTime ready <*> pure (dtiCreated dti)
    pullDuration xs = error . mconcat $ ["pairDuration: unexpected input length ", show (length xs), ": ", show xs]

    -- draft starts at the first "mark as draft" event or when the PR is created
    draftStart = fromMaybe (dtiCreated dti) maybeFirstMarkDraft
    maybeFirstMarkDraft = firstJust markDraftTime $ dtiEvents dti
    maybeFirstMarkReady = firstJust markReadyTime $ dtiEvents dti

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n rest
  where (chunk, rest) = splitAt n xs
