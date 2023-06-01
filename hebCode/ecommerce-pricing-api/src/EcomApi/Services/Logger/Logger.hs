{-
  Module      : Logger
  Description : Logging module for formatted logging of the format -
                timestamp  [log_level]  "message"
-}

module EcomApi.Services.Logger.Logger(getLogger,Logger(..)) where

import Data.Time
import Control.Monad(when)
import EcomApi.Services.Logger.Types
import EcomApi.Core.Config.Types


-- | Logger newtype wrapper for a function that takes a log level and a message and prints that to console
newtype Logger = Logger{
    runLog:: LogLevel -> String -> IO ()
}


-- | returns a Logger with given config applied to it
getLogger config = Logger{
    runLog = outputLog (logType config) (logLevel config)
}


-- | print log to console
printToConsole :: LogLevel -> String -> IO ()
printToConsole priority  message = do
      time <- getCurrentTime
      putStrLn (logFormat time priority message)




-- | print log to file
printToFile :: LogLevel -> String -> IO ()
printToFile priority message = do
      time <- getCurrentTime
      today <- utctDay <$> getCurrentTime
      let logfile = show today
      appendFile logfile (logFormat time priority message)



-- | choose whether to print to file or console
outputLog :: [LogType] -> LogLevel -> LogLevel -> String -> IO ()
outputLog [] _ _ _= return ()
outputLog (logtype:xs) maxLogLevel logLevel message= do
    when (logLevel>=maxLogLevel) $
        case logtype of
            Console -> do
                printToConsole logLevel message
                outputLog xs  logLevel maxLogLevel message
            File -> do
                printToFile logLevel message
                outputLog xs logLevel maxLogLevel message



-- | formatting output log string
logFormat :: UTCTime -> LogLevel -> String -> String
logFormat time logLevel message = do
    show time  ++ " " ++ "[" ++ show logLevel ++ "]" ++ " " ++ message
