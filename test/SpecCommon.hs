module SpecCommon where

import Data.Maybe
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Stack

-- | Parses @UTCTime@ from an ISO8601 string: @2022-12-31T23:59:59Z@.
-- Fails if the string is not in the correct format.
utcTime :: HasCallStack => String -> UTCTime
utcTime = fromJust . iso8601ParseM
