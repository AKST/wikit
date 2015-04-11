module Util.Strings (ddmmyyDate, hhmmssTime) where

import qualified Data.Date.UTC as Date
import qualified Data.Date as Date
import qualified Data.Time as Time
import qualified Data.Enum as Enum
import qualified Data.Int as Int

ddmmyyDate :: Date.Date -> String
ddmmyyDate date = day ++ " / "  ++ month ++ " / " ++ year where
  month = showEnum (Date.month date)
  year  = case (Date.year date) of
    Date.Year n -> show n
  day   = case Date.dayOfMonth date of
    Date.DayOfMonth n -> show n

hhmmssTime :: Date.Date -> String
hhmmssTime _ = "N/A"
-- hhmmssTime date = hour ++ ":" ++ minute ++ ":" ++ second ++ "." ++ ms where
--   hour   = showEnum (Date.hourOfDay date)
--   minute = showEnum (Date.minuteOfHour date)
--   second = showEnum (Date.secondOfMinute date)
--   ms     = showEnum (Date.millisecondOfSecond date)

showEnum :: forall n. (Enum.Enum n) => n -> String
showEnum n = show (Int.fromNumber (Enum.fromEnum n))
