module WorkDiffTime (
  -- * Types
    WorkDiffTime

  -- * Constructors
  , diffWorkTime

  -- * Getters
  , regular
  , work
  ) where

import Data.Time

type UTCPeriod = (UTCTime, UTCTime)

-- | Represents a time duration calculated between two time points; can return
-- both the "regular" duration (including all days) and "work" duration
-- (excluding all weekends).
--
-- We store a list of periods because we have to know the actual dates and times
-- in order to filter out any weekends for "work time".
-- TODO rename to more generic
newtype WorkDiffTime = WorkDiffTime UTCPeriod
  deriving Eq

instance Show WorkDiffTime where
  show (WorkDiffTime dt) = mconcat ["WorkDiffTime ", show dt]

diffWorkTime :: UTCTime -> UTCTime -> WorkDiffTime
diffWorkTime = curry WorkDiffTime

-- | Returns the "regular" time difference, including all days.
regular :: WorkDiffTime -> NominalDiffTime
regular (WorkDiffTime (to, from)) = diffUTCTime to from

-- | Returns the "work" time difference, that is excluding all weekends.
work :: WorkDiffTime -> NominalDiffTime
work (WorkDiffTime (to, from)) = fullDiff - weekends
  where
    fromDay = utctDay from
    toDay = utctDay to

    fullDiff = diffUTCTime to from
    -- TODO should we care about the local timezone even though the times are UTC?
    weekends = (* nominalDay) . fromIntegral . length . filter isWeekend . fmap dayOfWeek $ [fromDay..toDay]

    isWeekend Saturday = True
    isWeekend Sunday = True
    isWeekend _ = False
