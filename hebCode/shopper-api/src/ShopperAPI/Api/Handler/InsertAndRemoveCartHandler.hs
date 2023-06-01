{-|
    Module          : ShopperAPI.Api.Handler.CartHandler
    Description     : Handler for /addproducttocart and /removefromcart
-}

{-# LANGUAGE OverloadedStrings     #-}

module ShopperAPI.Api.Handler.InsertAndRemoveCartHandler where


import qualified    Data.Text                         as T
import              GHC.Generics

import              Servant
import              Data.ByteString.Char8             as P (pack, unpack)
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Reader 
import              Servant.Auth.Server
import              Data.Text.Encoding                (encodeUtf8)
import Control.Exception (throw)

import              ShopperAPI.Services.Database.Postgres
import              ShopperAPI.Services.Database.Types
import              ShopperAPI.Api.MiddleWare.Auth.Types
import              ShopperAPI.Api.Types
import              ShopperAPI.Core.Config.Types

import qualified    Data.ByteString.Lazy.UTF8         as B


-- | Handler function for adding Product  to Cart endpoint
addToCart:: Int -> AccessData -> ApiHandler(ApiResponse String)
addToCart proid accessData = do
    dbOps <- asks envDbOps
    let loginId = ShopperAPI.Api.MiddleWare.Auth.Types.id accessData
    inserted <- liftIO $ insertProductCart dbOps proid loginId  1 
    case inserted of 
        Right 1 -> return $ ApiResponse"The product has been added to cart" Nothing 200
        Left msg -> throwError err400{errBody = "insertion failed"}


-- | Handler Function for Deleting the product from the Cart endpoint
removeFromCart :: Int -> ApiHandler(ApiResponse String)
removeFromCart proid = do 
    dbOps <- asks envDbOps
    if proid >0 
        then do 
            res <- liftIO $ removeProductCart dbOps proid 
            case res of 
                Right 1 -> return $ ApiResponse "The product has been removed from cart " Nothing 200 
                Left msg -> throwError err500 {errBody = "removing the product failed"}
        else throwError err500{ errBody = "invalid productid "}
        