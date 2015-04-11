module Text.Date (ddmmyyDate, hhmmssTime) where


import qualified Data.Date.UTC as Date
import qualified Data.Date as Date
import qualified Data.Time as Date
import qualified Data.Enum as Enum
import qualified Data.Int as Int

import qualified Text.Number as Number 


ddmmyyDate :: Date.Date -> String
ddmmyyDate date = day ++ " / "  ++ month ++ " / " ++ year where
  month = show (Enum.fromEnum (Date.month date) + 1)
  year  = case (Date.year date) of
    Date.Year n -> show (Int.toNumber n)
  day   = case Date.dayOfMonth date of
    Date.DayOfMonth n -> show (Int.toNumber n)


hhmmssTime :: Date.Date -> String
hhmmssTime date = hour ++ ":" ++ minute ++ ":" ++ second where

  hour = case Date.hourOfDay date of
    Date.HourOfDay h -> 
      h # Int.toNumber # Number.prefixedZeros 2 

  minute = case Date.minuteOfHour date of
    Date.MinuteOfHour m -> 
      m # Int.toNumber # Number.prefixedZeros 2 

  second = case Date.secondOfMinute date of
    Date.SecondOfMinute s -> 
      s # Int.toNumber # Number.prefixedZeros 2 


