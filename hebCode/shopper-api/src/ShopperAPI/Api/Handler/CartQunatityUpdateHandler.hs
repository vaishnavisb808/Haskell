{-# LANGUAGE OverloadedStrings     #-}

module              ShopperAPI.Api.Handler.CartQunatityUpdateHandler where

import              Servant
import              Data.Maybe
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Reader
import              Database.PostgreSQL.Simple

import              ShopperAPI.Services.Database.Types
import              ShopperAPI.Core.Config.Types
import qualified    ShopperAPI.Api.MiddleWare.Auth.Types as MT
import              ShopperAPI.Core.Types
import              ShopperAPI.Api.Types
import              ShopperAPI.Core.Utils
import              ShopperAPI.Services.Database.CartQuantityUpdateDB
import              ShopperAPI.Services.Logger.Logger    
import              ShopperAPI.Services.Logger.Types    

-- handler function for endpoint cartquantityupdate
cartQuantityUpdate ::Int ->Maybe Int->MT.AccessData -> ApiHandler( ApiResponse String)
cartQuantityUpdate productId updateQuantity dataAccess= withTimeLog CartQunatityHandle $ do
    let logger = cartQunatityHandleLog 
    let userId = MT.id dataAccess :: Int
    dbOps <- asks envDbOps 
    if isJust updateQuantity 
        then do
            qunatityLimit <- liftIO $ quantityLimit dbOps productId
            case qunatityLimit of
                Right quantity ->
                 if quantity < fromJust updateQuantity
                    then 
                        throwError $ jsonError500 "Maximum limit reached"
                    else do
                        availableProduct <- liftIO $ productstatuscheck dbOps productId
                        case availableProduct of
                            Right availStock ->
                                if availStock < fromJust updateQuantity
                                    then
                                        throwError $
                                            jsonError500 "Required quantity not available in stock"
                                    else do
                                        updatedRow <-liftIO $ 
                                                        cartqunatityupdate dbOps userId updateQuantity productId
                                        case updatedRow of
                                            Right uRows->do
                                                let logMessage= "Item quantity updated "
                                                logger INFO logMessage 200
                                                return $
                                                    ApiResponse "Item quantity has been updated" 
                                                                Nothing 
                                                                200
                                            Left er ->do
                                                let logMessage= "unable to update cart quantity"
                                                logger INFO logMessage 500
                                                throwError $
                                                                    jsonError500 "Unable to process request"
                            Left er ->do 
                                let logMessage= "stock count check failed"
                                logger INFO logMessage 500
                                throwError $
                                        jsonError500 "stock count check failed" 
                Left er ->do 
                    let logMessage= "purchase limit check in category failed"
                    logger INFO logMessage 500
                    throwError $
                        jsonError500 "purchase limit check in category failed"
        else  throwError $ jsonError500 "quantity not given"