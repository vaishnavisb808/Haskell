module EcomApi.Api.Middleware.Auth.Auth where

import           Control.Exception                     (SomeException, handle)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Reader            (asks)
import           Crypto.BCrypt                         (validatePassword)
import           Data.ByteString.Char8                 as P (pack, unpack)
import           Data.Text.Encoding                    (encodeUtf8)
import           EcomApi.Api.Middleware.Auth.Types     (AppData (AppData))
import           EcomApi.Core.Types                    (UserInfo (..))
import           EcomApi.Services.Database.GetAuthData (getAuthData)
import           EcomApi.Services.Database.Types       (DbOps, getUserInfo)
import           Servant                               (BasicAuthCheck (BasicAuthCheck),
                                                        BasicAuthData (basicAuthPassword, basicAuthUsername),
                                                        BasicAuthResult (Authorized, Unauthorized))


authcheck  :: DbOps -> BasicAuthCheck AppData
authcheck dbOps      = BasicAuthCheck $ \basicAuthData -> do
    let username  = P.unpack (basicAuthUsername basicAuthData)
        password  =   basicAuthPassword basicAuthData
    response      <- liftIO $ getUserInfo dbOps username
    case response of
      Left msg        -> return Unauthorized
      Right response  -> do
        let data'     = response
            appid     = EcomApi.Core.Types.app_id data'
            appname   = EcomApi.Core.Types.app_name data'
            dbPassword    = P.pack $ EcomApi.Core.Types.password data'
        let authorization = validatePassword dbPassword password
        if authorization
          then return $ Authorized $ AppData (show appid) appname
          else return Unauthorized



