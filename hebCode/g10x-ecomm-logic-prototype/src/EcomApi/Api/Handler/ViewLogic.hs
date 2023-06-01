{-|
    Module : EcomApi.Api.Handler.ViewLogic
    Description : Handler for /viewlogic
    Handler for /viewlogic

-}

{-# LANGUAGE OverloadedStrings #-}

module EcomApi.Api.Handler.ViewLogic(getLogicHandler) where

import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Reader          (asks)
import           Data.Maybe                          (fromMaybe)
import           Data.Time                           (UTCTime, getCurrentTime)
import           EcomApi.Api.Middleware.Auth.Types   (AppData)
import           EcomApi.Api.Types                   as API (ApiHandler,
                                                             ApiResponse (ApiResponse),
                                                             Response)
import           EcomApi.Core.Config.Types           (Env, envDbOps,
                                                      envRequestId)
import           EcomApi.Core.Transformers           (convertLogicListToResponse)
import           EcomApi.Core.Utils                  (jsonError400,
                                                      jsonError404,
                                                      jsonError500)
import           EcomApi.Services.Database.Types     (DbOps (DbOps), getupcData,
                                                      viewLogic)
import           EcomApi.Services.Database.ViewLogic (getLogicsByUpc)
import           EcomApi.Services.Logger.Logger      (logToChannel,
                                                      viewLogicLog, withTimeLog)
import           EcomApi.Services.Logger.Types       (Endpoint (ViewLogic),
                                                      HttpMethod (GET),
                                                      Log (..),
                                                      LogLevel (ERROR, INFO))
import           Servant                             (Handler, throwError)




type Logger = LogLevel -> String -> Integer -> API.ApiHandler ()

-- | Handler function for viewLogic endpoint.
getLogicHandler :: AppData
                -> Integer
                -> Maybe Int
                -> ApiHandler (ApiResponse API.Response)
getLogicHandler user upc lastId = withTimeLog user ViewLogic $ do
    let logger = viewLogicLog user
    -- UPC must be a positive number
    if upc >= 0 && upc < 1000000000000
        then do
            dbOps <- asks envDbOps
            -- pageKey must be a postive integer
            upcData <- liftIO $ getupcData dbOps upc
            case lastId of
                (Just pgKey) -> getLogicForUPC dbOps logger upc pgKey
                Nothing    ->
                    case upcData of
                      Left err -> do
                          logger ERROR ("Failed to fetch data for upc"++show err) 500
                          throwError $ jsonError500 "failed to fetch data for upc"
                      Right [] -> do
                          logger INFO ("No data found for upc"++show upc) 404
                          throwError $ jsonError404 "No data found for UPC"
                      Right [(_,logicId)] -> getLogicForUPC dbOps logger upc logicId


        else do
            logger INFO ("invalid upc "++show upc) 400
            throwError $jsonError400 "Invalid UPC,UPC must be a positive number , max 12 digits"
  where
    getLogicForUPC :: DbOps -> Logger -> Integer -> Int -> ApiHandler (ApiResponse API.Response)
    getLogicForUPC dbOps logger upc pageKey = do
        -- get logic corresponding to UPC from database
        view <- liftIO $ viewLogic dbOps upc pageKey
        currentTime <- liftIO  getCurrentTime
        case view of
            Left err -> do
                
                logger ERROR (show err) 500
                throwError $jsonError500 "falied to fetch data for upc"
            Right ([],nextPageKey) ->  do
                logger INFO ("No data found for upc " ++ show upc) 404
                if lastId /= Nothing
                   then  throwError $
                            jsonError404 $ "No data found for pageKey = "
                                         ++ show pageKey
                                         ++ " for upc "
                                         ++ show upc
                   else throwError $
                            jsonError404 $ "No data found for upc "
                                         ++ show upc
            Right (logicList,nextPageKey) ->
                return $ ApiResponse (convertLogicListToResponse logicList
                                                                 currentTime
                                                                 nextPageKey
                                                                 (lastId /= Nothing)
                                     )
                                     Nothing
                                     200
