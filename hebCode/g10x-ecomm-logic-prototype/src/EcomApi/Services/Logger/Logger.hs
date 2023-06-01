{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Module      : Logger
  Description : Logging module for formatted logging
-}

module EcomApi.Services.Logger.Logger where


import           Control.Concurrent.Chan           (Chan, readChan, writeChan)
import           Control.Lens
import           Control.Monad                     (forever, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.ByteString.Lazy.Char8        as BSC8 (unpack)
import           Data.Text                         as T
import           Data.Time                         (UTCTime, diffUTCTime,
                                                    getCurrentTime, utctDay)
import           EcomApi.Api.Middleware.Auth.Types (AppData)
import           EcomApi.Api.Types                 (ApiHandler)
import qualified EcomApi.Core.Config.Types         as Config (Configuration (..),
                                                              Env (..))
import           EcomApi.Services.Logger.Types     (Endpoint (ModifyLogic, ViewLogic),
                                                    HttpMethod (GET, POST),
                                                    JsonEventLog (JsonEventLog),
                                                    JsonLog (JsonLog),
                                                    JsonLogEvent (JsonLogEvent),
                                                    JsonLogHttp (JsonLogHttp),
                                                    JsonLogUrl (JsonLogUrl),
                                                    Log (..), LogLevel (INFO),
                                                    LogType (Console, File),
                                                    Logger (Logger), runLog)


-- | returns a Logger with given config applied to it
getLogger :: Config.Configuration -> Logger
getLogger config = Logger
    { runLog = outputLog (Config.logTypes config) (Config.logLevel config) }


logLoop :: Chan Log -> Logger -> IO()
logLoop channel logger = forever $ do
    log <- readChan channel
    runLog logger log

-- | print log to console
printToConsole :: Log -> IO ()
printToConsole log = do
      time <- getCurrentTime
      putStrLn (logFormat time log)


-- | print log to File
printToFile :: Log -> IO ()
printToFile log  = do
      time <- getCurrentTime
      today <- utctDay <$> getCurrentTime
      let logfile = show today
      appendFile logfile (logFormat time log)



-- | choose whether to print to file or console
outputLog :: [LogType] -> LogLevel -> Log -> IO ()
outputLog [] _ _  = return ()
outputLog (logtype:xs) maxLogLevel log = do
    when ( logLevel log >= maxLogLevel ) $
        case logtype of
            Console                              -> do
                printToConsole log
                outputLog xs  maxLogLevel log
            File -> do
                printToFile log
                outputLog xs  maxLogLevel log



-- | formatting output log string
logFormat :: UTCTime -> Log -> String
logFormat time (Log lvl msg path reqMethod reqStatus reqId user) = do
    let urlPath = case path of
                    Just ViewLogic   -> Just "/viewlogic"
                    Just ModifyLogic -> Just "/modifylogic"
                    Nothing          -> Nothing
    BSC8.unpack $ encode
        ( JsonLog (show lvl)
                  msg
                  ( JsonLogHttp  (show <$> reqMethod)
                                 reqStatus
                                 (Just ( JsonLogUrl urlPath ))
                  )
        )
logFormat time (EventLog lvl msg evtName evtOutCome) = BSC8.unpack $ encode
        ( JsonEventLog (show lvl)
                  msg
                  ( JsonLogEvent evtName
                                 evtOutCome
                  )
        )


-- helper function to log message
logToChannel :: Log -> ApiHandler ()
logToChannel  log  = do
    channel<-asks Config.envLogChannel
    liftIO $ writeChan channel log

{- Execute an API Handler along with a measure of time taken to do so.
   The time taken is logged with INFO Level in seconds
-}
withTimeLog :: AppData -> Endpoint -> ApiHandler a -> ApiHandler a
withTimeLog usr path action = do
    start <- liftIO getCurrentTime
    !result <- action
    stop <- liftIO getCurrentTime
    let timeDiff = diffUTCTime stop start
    case path of
      ViewLogic -> viewLogicLog usr
                                INFO
                                ("Request completed in : "
                                ++show(diffUTCTime stop start))
                                200
      ModifyLogic -> modifyLogicLog usr
                                    INFO
                                    ("Request completed in : "
                                    ++show(diffUTCTime stop start))
                                    200

    let integerTime = round timeDiff
    return result



-- | create a Log with request metadate and push that to log queue
viewLogicLog :: AppData -> LogLevel -> String -> Integer -> ApiHandler ()
viewLogicLog appData loglevel logmessage statusCode  = do
    reqId <- asks Config.envRequestId
    logToChannel Log
        { logLevel = loglevel
        , logMessage = logmessage
        , endpoint = Just ViewLogic
        , request_method = Just GET
        , request_status_code = Just statusCode
        , request_id = reqId
        , user = Just appData
        }



-- | create a log with all request metadata and write that to log queue
modifyLogicLog :: AppData -> LogLevel -> String -> Integer -> ApiHandler ()
modifyLogicLog appData loglevel logmessage statusCode  = do
    reqId <- asks Config.envRequestId
    logToChannel Log
        { logLevel = loglevel
        , logMessage = logmessage
        , endpoint = Just ModifyLogic
        , request_method = Just GET
        , request_status_code = Just statusCode
        , request_id = reqId
        , user = Just appData
        }
