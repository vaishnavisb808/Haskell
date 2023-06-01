{-
    Module : ShopperAPI.Api.MiddleWare.TokenGeneration
    Description : Function generate unique token
-}

module           ShopperAPI.Api.MiddleWare.Auth.TokenGeneration where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8                as P (pack, unpack)
import qualified Data.ByteString.Lazy.UTF8            as B
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           GHC.Generics
import qualified Network.Wai                          as ShopperAPI.Api
import           Servant
import           Servant.Auth.Server
import           ShopperAPI.Api.MiddleWare.Auth.Types
import           ShopperAPI.Api.Types                 as AT
import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Core.Types                as CT
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types
import           Text.Read                            (readMaybe)

-- | Function for generating the token.
generateToken :: LoginInfo -> ApiHandler String
generateToken dbdata = do
    getConfig   <- asks envConfig
    let id = CT.login_id dbdata
        owner = CT.role dbdata
        roleIdentity = readMaybe owner :: Maybe Role
    case roleIdentity of
        Just x  -> createToken getConfig (AccessData id x)
        Nothing -> throwError $ jsonError400 "invalid role"

createToken :: Configuration -> AccessData -> ApiHandler String
createToken getConfig tokenData = do
    token      <- liftIO $ makeJWT tokenData jwtCfg Nothing
    case token of
        Left e  -> throwError $jsonError400 "token generation failed"
        Right v -> return $ B.toString v

  where
    myKey   = fromSecret (encodeUtf8 (T.pack  (authKey getConfig)))
    jwtCfg  = defaultJWTSettings myKey
    cfg     = defaultCookieSettings :. jwtCfg :. EmptyContext
