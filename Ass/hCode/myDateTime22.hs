import Data.Time
import Data.Time.Clock (UTCTime(UTCTime))
import Data.Time (Day, DiffTime)

getCurrentTime :: IO() UTCTime

data UTCTime = UTCTime
   {utctDay :: Day
   , utctDayTime :: DiffTime}

toGregorian :: Day -> (Integer, Int, Int)  -- year, month, day of month
fromGregorian :: Integer -> Int -> Int -> Day  -- year, month, day of month

timeToTimeOfDay :: DiffTime -> TimeOfDay
timeOfDayToTime :: TimeOfDay -> DiffTime

data TimeOfDay = TimeOfDay
  { todHour :: Int
  , todMin  :: Int
  , todSec  :: Pico
  }

midnight :: TimeOfDay
midday :: TimeOfDay