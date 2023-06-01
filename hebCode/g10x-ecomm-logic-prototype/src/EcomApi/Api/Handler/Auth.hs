module EcomApi.Api.Handler.Auth where

import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.UTF8         as B
import qualified Data.Text                         as T
import           Data.Text.Encoding                (encodeUtf8)
import           EcomApi.Api.Middleware.Auth.Types (AppData)
import           EcomApi.Api.Types                 (ApiHandler,
                                                    ApiResponse (ApiResponse))
import           EcomApi.Core.Config.Types         (authKey,envConfig)
import           EcomApi.Core.Utils                (jsonError500)
import           Servant                           (Context (EmptyContext, (:.)),
                                                    throwError)
import           Servant.Auth.Server               (defaultCookieSettings,
                                                    defaultJWTSettings, makeJWT,fromSecret)


-- | Handler function for /auth endpoint.
getToken :: AppData -> ApiHandler (ApiResponse String)
getToken appdata = do
  getConfig   <- asks envConfig
  let myKey   = fromSecret (encodeUtf8 (T.pack  (authKey getConfig)))
      jwtCfg  = defaultJWTSettings myKey
      cfg     = defaultCookieSettings :. jwtCfg :. EmptyContext
  etoken      <- liftIO $ makeJWT appdata jwtCfg Nothing
  case etoken of
    Left e -> do
      -- logger ERROR (show e) 500
      throwError $jsonError500 "token generation failed, try again"
    Right v -> do
      let token = B.toString v
      return $ ApiResponse token Nothing 200
