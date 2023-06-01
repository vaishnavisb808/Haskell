{-|
    Module : EcomApi.Api.Handler.ViewLogic
    Description : Handler for /viewlogic
    Handler for /viewlogic

-}

{-# LANGUAGE OverloadedStrings #-}

module EcomApi.Api.Handler.ViewLogic(getLogicHandler) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString.Lazy.Internal       as B
import           Data.Time
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           EcomApi.Api.Types                   as API
import           EcomApi.Core.Config.Types
import           EcomApi.Core.Transformers
import           EcomApi.Core.Utils
import           EcomApi.Services.Database.Types
import           EcomApi.Services.Database.ViewLogic
import           Servant
import           EcomApi.Services.Logger.Types
import           EcomApi.Services.Logger.Logger

-- | Handler function for viewLogic endpoint.
getLogicHandler :: Integer -> Maybe Integer -> ApiHandler (ApiResponse API.Response)
getLogicHandler upc page = withTimeLog $ do
    if upc >= 0
        then do
            getdbops <- asks envDbOps
            currentTime <- liftIO  getCurrentTime
            let page' = fromMaybe 0 page
            if page'<0 
                then throwError $jsonError400 $ "Invalid page " ++ show page'
                else do
                    view <- liftIO $ viewLogic getdbops upc page'
                    case view of
                        Left err -> do
                            logger ERROR (show err)
                            throwError $jsonError500 "falied to fetch data for upc"
                        Right [] ->  do
                            let logMessage="No data found for upc " ++ show upc
                            logger INFO logMessage
                            if page' > 0 
                               then  throwError $jsonError404 $ "No data found on page " ++ show page'++ " for upc "++ show upc
                               else throwError $jsonError404 $ "No data found for upc " ++ show upc
                        Right logicList ->  do
                            let logMessage = "Retrived data successfully for " ++ show upc
                            logger INFO logMessage 
                            return $ ApiResponse (convertLogicListToResponse logicList currentTime page') Nothing 200
        else do
            let logMessage="Incorrect UPC"
            logger INFO logMessage
            throwError $jsonError400 logMessage
