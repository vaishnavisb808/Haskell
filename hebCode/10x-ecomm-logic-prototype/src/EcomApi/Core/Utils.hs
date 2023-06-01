{-|
    Module : EcomApi.Core.Utils
    Description : Functions to produce json formatted response body along with
                  appropriate status code for error messages
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module EcomApi.Core.Utils where
import           Control.Concurrent
import           EcomApi.Services.Logger.Types
import           Control.Monad.IO.Class
import           EcomApi.Api.Types                   as API
import           EcomApi.Core.Config.Types
import           EcomApi.Services.Logger.Types
import           EcomApi.Services.Logger.Logger
import           Control.Monad              (when)
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.ByteString.Lazy       (ByteString, fromStrict)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Servant
import           Data.Time

-- | convert String to Lazy ByteString
stringToLazyBytestring :: String -> ByteString
stringToLazyBytestring = fromStrict . encodeUtf8 . T.pack

{-| Helper function to produce json formatted response body along with
    appropriate status code for error messages
-}
body code msg= object ["code".=(code::Int),"error".=msg,"result".=Null]

-- |400 : Bad Request.The server can’t return a response due to an error on the client’s end
jsonError400::String->ServerError
jsonError400  msg = err400{errBody=encode (body 400 msg)
                    , errHeaders = [("Content-Type", "application/json")]
                    }

{-| 401 : This is returned by the server when the target resource lacks valid authentication
    credentials. -}
jsonError401::String->ServerError
jsonError401  msg = err401{ errBody=encode (body 401 msg)
                    , errHeaders = [("Content-Type", "application/json")]
                    }

{-| 404 : The requested resource was not found. The requested resource does not exist, and the
   server does not know if it ever existed -}
jsonError404::String->ServerError
jsonError404  msg = err404{ errBody=encode (body 404 msg)
                    , errHeaders = [("Content-Type", "application/json")]
                    }

-- | 500 : Something went wrong on the server and the requested resource was not delivered
jsonError500::String->ServerError
jsonError500  msg = err500{ errBody=encode (body 500 msg)
                , errHeaders = [("Content-Type", "application/json")]
                }


-- helper function to log message
logger::LogLevel -> String -> ApiHandler ()
logger logLevel message = do
    channel<-asks envLogChannel
    liftIO $ writeChan channel $ Log logLevel message


{- Execute an API Handler along with a measure of time taken to do so.
   The time taken is logged with INFO Level in seconds
-}
withTimeLog :: ApiHandler a -> ApiHandler a
withTimeLog action = do
    start <- liftIO getCurrentTime
    !result <- action
    stop <- liftIO getCurrentTime
    logger INFO $"Request completed in : "++show(diffUTCTime stop start)
    return result