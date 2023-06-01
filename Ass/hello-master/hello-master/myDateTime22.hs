import Data.Time
import Data.Time.UTCTime
import Data.Time (Day, DiffTime)

getCurrentTime :: IO() UTCTime
data UTCTime = UTCTime
   {utctDay :: Day
   , utctDayTime :: DiffTime}


