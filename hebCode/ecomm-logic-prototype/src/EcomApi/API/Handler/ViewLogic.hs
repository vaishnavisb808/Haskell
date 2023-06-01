{-|
    Module : EcomApi.API.Handler.ViewLogic
    Description : Handler for /viewlogic
    Handler for /viewlogic

-}
{-# LANGUAGE OverloadedStrings #-}
module EcomApi.API.Handler.ViewLogic(getLogicEndPoint) where

import           EcomApi.Core.Utils
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString.Lazy.Internal       as B
import           Data.Time
import           Database.PostgreSQL.Simple
import           EcomApi.API.Types                   as API
import           EcomApi.Core.Config.Types
import           EcomApi.Core.Transformers
import           EcomApi.Services.Database.Types
import           EcomApi.Services.Database.ViewLogic
import           Servant


-- | Handler function for viewLogic endpoint.
getLogicEndPoint :: Integer ->  ApiHandler (ApiResponse API.Response)
getLogicEndPoint upc = do
    if upc > 0
        then do
            getdbops <- asks envDbOps
            currentTime <- liftIO $ getCurrentTime
            view <- liftIO $ viewLogic getdbops upc
            case view of
                Left e -> throwError $jsonError500 "falied to fetch data for upc"
                Right [] ->  throwError $jsonError404 "No data found for upc"
                Right logic -> return $ ApiResponse (finalResponse logic currentTime) Nothing 200
        else
            throwError $jsonError404 "Invalid upc,upc must be a positive number"

