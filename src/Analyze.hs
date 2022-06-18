module Analyze
  ( DraftTimeInput(..)
  , draftDuration
  ) where

import Control.Applicative ((<|>))
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
-- * there can't be 2+ events of the same type in a row.
draftDuration :: (HasCallStack, DraftTimeInput a) => a -> NominalDiffTime
draftDuration dti = getSum . mconcat . fmap Sum $ draftDurations
  where
    draftDurations = map pairDuration $ adjacentPairs stateTransitions

    stateTransitions = catMaybes
      $ Just (StateTransition Created (dtiCreated dti))
      : map parseStateEvent (dtiEvents dti)
      ++ [StateTransition Merged <$> dtiMerged dti]

    parseStateEvent :: PullEvent -> Maybe StateTransition
    parseStateEvent e
      =   StateTransition MarkedDraft <$> markDraftTime e
      <|> StateTransition MarkedReady <$> markReadyTime e

    pairDuration :: HasCallStack => (StateTransition, StateTransition) -> NominalDiffTime
    pairDuration (StateTransition MarkedDraft draft, StateTransition MarkedReady ready) = diffUTCTime ready draft
    pairDuration (StateTransition MarkedReady _, StateTransition MarkedDraft _) = 0
    pairDuration (StateTransition MarkedDraft draft, StateTransition Merged merged) = diffUTCTime merged draft
    pairDuration (StateTransition MarkedReady _, StateTransition Merged _) = 0
    pairDuration (StateTransition Created created, StateTransition MarkedReady ready) = diffUTCTime ready created
    pairDuration (StateTransition Created _, StateTransition Merged _) = 0
    pairDuration (StateTransition Created _, StateTransition MarkedDraft _) = 0
    pairDuration (x, c@(StateTransition Created _)) = error . mconcat $ ["pairDuration: impossible ", show c, " after ", show x]
    pairDuration (m@(StateTransition Merged _), x) = error . mconcat $ ["pairDuration: impossible ", show x, " after ", show m]
    pairDuration (x@(StateTransition MarkedDraft _), y@(StateTransition MarkedDraft _)) = error . mconcat $ ["pairDuration: unexpected pair of MarkedDraft in a row: ", show (x, y)]
    pairDuration (x@(StateTransition MarkedReady _), y@(StateTransition MarkedReady _)) = error . mconcat $ ["pairDuration: unexpected pair of MarkedReady in a row: ", show (x, y)]

-- | Describes state transition events that are interesting for @draftDuration@.
data StateTransitionEvent = Created | MarkedDraft | MarkedReady | Merged
  deriving Show

data StateTransition = StateTransition
  { stEvent :: StateTransitionEvent
  , stTime :: UTCTime
  }
  deriving Show

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs xs = zip xs (tail xs)
