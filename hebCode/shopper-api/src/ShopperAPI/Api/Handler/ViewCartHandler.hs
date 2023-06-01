{-|
    Module          : ShopperAPI.Api.Handler.ViewCartHandler
    Description     : Handler for /viewcart
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}


module              ShopperAPI.Api.Handler.ViewCartHandler where


import qualified    Data.Text                            as T
import              GHC.Generics
import              ShopperAPI.Api.Types                 as AT
import              ShopperAPI.Api.MiddleWare.Auth.Types as MT
import              Servant
import              Data.ByteString.Char8                as P (pack, unpack)  
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Reader 
import              ShopperAPI.Core.Config.Types 
import              Servant.Auth.Server
import qualified    Data.ByteString.Lazy.UTF8            as B
import              Data.Text.Encoding                (encodeUtf8)
import              ShopperAPI.Core.Transformers (convertViewCartCoreToApi)
import              ShopperAPI.Services.Database.ViewCart
import              ShopperAPI.Services.Database.Postgres 
import              ShopperAPI.Core.Utils
import              ShopperAPI.Services.Database.Types
import              ShopperAPI.Core.Types                as CT

-- | Handler function for viewcart endpoint.

viewCartHandler ::  AccessData -> ApiHandler ( ApiResponse [AT.ProductCart])
viewCartHandler userinfo = do
    
    let loginid = MT.id userinfo 
    dbOps <- asks envDbOps
    if loginid <0
        then throwError $err400 {errBody = "wrong loginid"}
        else do
            dbresult <- liftIO $  viewCartInfo dbOps loginid
            case dbresult of
                Left e -> throwError $ jsonError404  "sql error"
                Right [] ->  throwError $jsonError404  "No data to display ,Cart is empty"
                Right r ->   return $ ApiResponse (convertViewCartCoreToApi <$> r) Nothing 200
                       
                        
                      