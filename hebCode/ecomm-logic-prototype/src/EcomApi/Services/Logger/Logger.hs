{-|
  Module      : Logger
  Description : Logging module for formatted logging
-}

module EcomApi.Services.Logger.Logger(getLogger,Logger(..),logLoop,Log(..)) where


import           Control.Concurrent.Chan
import           Control.Monad                 (forever, when)
import           Control.Monad.Trans.Reader
import           Data.Time                     (UTCTime, getCurrentTime,
                                                utctDay)
import qualified EcomApi.Core.Config.Types     as Config (Configuration (..))
import           EcomApi.Services.Logger.Types (Log (..), LogLevel (..),
                                                LogType (..), Logger (..))


-- | returns a Logger with given config applied to it
getLogger :: Config.Configuration -> Logger
getLogger config = Logger
    { logDebug = runLog DEBUG
    , logInfo  = runLog INFO
    , logWarn  = runLog WARN
    , logError = runLog ERROR
    }
  where
    runLog :: LogLevel -> String -> IO ()
    runLog = outputLog (Config.logTypes config) (Config.logLevel config)

-- | continously read from the channel and log messages appropriately
logLoop :: Chan Log -> Logger -> IO()
logLoop channel logger = forever $ do
    log <- readChan channel
    case logLevel log of
      DEBUG -> logDebug logger $ logMessage log
      INFO  -> logInfo logger $ logMessage log
      WARN  -> logWarn logger $ logMessage log
      ERROR -> logError logger $ logMessage log

-- | print log to console
printToConsole :: LogLevel -> String -> IO ()
printToConsole priority  message = do
      time <- getCurrentTime
      print (logFormat time priority message)


-- | print log to file
printToFile :: LogLevel -> String -> IO ()
printToFile priority message = do
      time <- getCurrentTime
      today <- utctDay <$> getCurrentTime
      let logfile = show today
      appendFile logfile (logFormat time priority message)


-- | choose whether to print to file or console
outputLog :: [LogType] -> LogLevel -> LogLevel -> String -> IO ()
outputLog [] _ _ _ = return ()
outputLog (logtype:xs) maxLogLevel logLevel message= do
    when (logLevel>=maxLogLevel) $
        case logtype of
            Console                              -> do
                printToConsole logLevel message
                outputLog xs  logLevel maxLogLevel message
            File -> do
                printToFile logLevel message
                outputLog xs logLevel maxLogLevel message


-- | formatting output log string
logFormat :: UTCTime -> LogLevel -> String -> String
logFormat time logLevel message = do
    show time  ++ " " ++ "[" ++ show logLevel ++ "]" ++ " " ++ message ++ "\n"
