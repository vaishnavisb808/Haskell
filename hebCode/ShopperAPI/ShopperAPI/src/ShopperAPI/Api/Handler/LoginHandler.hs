{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings     #-}


module              ShopperAPI.Api.Handler.LoginHandler where


import qualified    Data.Text                         as T
import              GHC.Generics
import              ShopperAPI.Api.Types
import              Servant
import              Data.ByteString.Char8             as P (pack, unpack)
import              ShopperAPI.Services.Database.Types as DB
import              ShopperAPI.Services.Database.Postgres
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Reader 
import              ShopperAPI.Core.Config.Types
import              Servant.Auth.Server
import qualified    Data.ByteString.Lazy.UTF8         as B
import              Data.Text.Encoding                (encodeUtf8)


loginfunction :: Login ->ApiHandler( ApiResponse String)
loginfunction req = do
    dbOps <- asks envDbOps
    getConfig   <- asks envConfig
    let uname = ShopperAPI.Types.username req
    let passwd = ShopperAPI.Types.password req
    dbdata <- liftIO $ getMgrInfo dbOps uname passwd 
    case dbdata of
        Right dbdata -> do
            let id = DB.login_id dbdata
                owner = DB.role dbdata
                tokendata = AccessData id owner
                myKey   = fromSecret (encodeUtf8 (T.pack  (authKey getConfig)))
                jwtCfg  = defaultJWTSettings myKey
                cfg     = defaultCookieSettings :. jwtCfg :. EmptyContext
            etoken      <- liftIO $ makeJWT tokendata jwtCfg Nothing
            case etoken of
                Left e -> do
                            logger INFO ("shjbjf"++owner)
                            throwError err500 {errBody = "token generation failed"}
                Right v -> do
                            let token = B.toString v
                            return $ ApiResponse token Nothing 200
        Left msg -> throwError err500 {errBody = "Invalid Credentials"}