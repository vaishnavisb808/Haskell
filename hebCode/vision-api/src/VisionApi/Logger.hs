{-
  Module      : Logger
  Description : Logging module for formatted logging of the format -
                timestamp  [log_level]  "message"
-}

module VisionApi.Logger(getLogger,Logger(..)) where

import Data.Time
import Control.Monad.Trans.Reader
import Control.Monad(when)
import VisionApi.Config(Configuration(..))
import VisionApi.Logger.Types
import VisionApi.Logger.Internal

-- | Logger newtype wrapper for a function that takes a log level and a message and prints that to console
newtype Logger = Logger{
    runLog:: LogLevel -> String -> IO ()
}


-- | returns a Logger with given config applied to it
getLogger config = Logger{
    runLog = outputLog (logType config) (logLevel config)
}

