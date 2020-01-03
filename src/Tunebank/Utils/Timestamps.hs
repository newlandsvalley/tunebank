module Tunebank.Utils.Timestamps
  ( day2timestamp
  , time2timestamp
  , now2timestamp
  , today
  , fromDay
  , timeNow
  ) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Data.Text (Text, pack)

day2timestamp :: Day -> Text
day2timestamp  =
  pack . show . time2POSIXms . fromDay

time2timestamp ::  UTCTime -> Text
time2timestamp  =
    pack . show . time2POSIXms

now2timestamp :: IO Text
now2timestamp = do
  t <- getCurrentTime
  pure $ time2timestamp t

fromDay :: Day -> UTCTime
fromDay day =
  UTCTime day (secondsToDiffTime 0)

time2POSIXms :: UTCTime -> Int
time2POSIXms =
  (1000 *) . round . utcTimeToPOSIXSeconds

today :: IO Day
today = do
  t <- getCurrentTime
  pure $ utctDay t

timeNow :: IO UTCTime
timeNow = do
  day <- today
  pure $ fromDay day
