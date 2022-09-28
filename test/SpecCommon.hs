module SpecCommon where

import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Format.ISO8601
import GHC.Stack

import Database

-- | Parses @UTCTime@ from an ISO8601 string: @2022-12-31T23:59:59Z@.
-- Fails if the string is not in the correct format.
utcTime :: HasCallStack => String -> UTCTime
utcTime = fromJust . iso8601ParseM

mkPull :: Text -> Int -> Pull
mkPull repo number = Pull
    repo
    number
    ""
    ""
    ""
    False
    (UTCTime (fromOrdinalDate 2000 1) (secondsToDiffTime 0))
    Nothing
    ""
