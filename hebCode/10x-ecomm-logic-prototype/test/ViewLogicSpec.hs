{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module ViewLogicSpec where

import           Control.Concurrent
import           Data.Aeson
import           Data.Text                     as T
import           EcomApi.Api.Types
import           EcomApi.Core.Config.Types     (Configuration (..), Env (..))
import           EcomApi.Server                as Server
import           EcomApi.Services.Logger.Types (LogLevel (DEBUG, ERROR, INFO, WARN),
                                                LogType (Console, File))
import           MockDb
import           Network.HTTP.Types            (hAuthorization, methodGet)
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant
import           Servant.Auth.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher


testApp::Env->JWTSettings -> Application
testApp env jwtCfg=
    serveWithContext
        api
        (customFormatters Servant.:.jwtCfg :. defaultCookieSettings :. EmptyContext)
        (hoistServerWithContext
            api
            (Proxy :: Proxy '[CookieSettings, JWTSettings])
            (nt env)
            server
        )

token = "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ\
    \.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc\
    \203Q"


viewLogicSpec :: Env -> JWTSettings  -> Spec
viewLogicSpec env jwtCfg =
   with (return $ testApp env jwtCfg) $ do

    describe "GET /viewlogic" $ do
      it "should get all logic corresponding to upc" $do
          request methodGet
                "/viewlogic/1"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          200 {matchBody = MatchBody viewLogicResponseMatcher}

   where
     viewLogicResponseMatcher _ body =
         case (decode body::Maybe (ApiResponse Response)) of
           Just val -> if (code val) == 200
                           then Nothing
                           else Just "Response body mismatch"
           Nothing  -> Just $ "Invalid Response Schema. Failed to Parse"++show body
