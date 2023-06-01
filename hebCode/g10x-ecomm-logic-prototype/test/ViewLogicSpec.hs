{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module ViewLogicSpec where

import           Control.Concurrent
import           Data.Aeson
import           Data.Text                         as T
import           EcomApi.Api.Middleware.Auth.Auth
import           EcomApi.Api.Middleware.Auth.Types
import           EcomApi.Api.Types
import           EcomApi.Core.Config.Types         (Configuration (..),
                                                    Env (..))
import           EcomApi.Server                    as Server
import           EcomApi.Services.Logger.Types     (LogLevel (DEBUG, ERROR, INFO, WARN),
                                                    LogType (Console, File))
import           MockDatabase
import           Network.HTTP.Types                (hAuthorization, methodGet)
import qualified Network.Wai.Handler.Warp          as Warp
import           Servant
import           Servant.Auth.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

testApp::Env->JWTSettings -> Application
testApp env jwtCfg=
    serveWithContext
        api
        (customFormatters Servant.:.jwtCfg :. authcheck getMockDbOps :. defaultCookieSettings :. EmptyContext)
        (hoistServerWithContext
            api
            (Proxy :: Proxy '[CookieSettings, JWTSettings,BasicAuthCheck AppData])
            (nt env)
            server
        )

token = "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ\
    \.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc\
    \203Q"


viewLogicSpec :: Env -> JWTSettings  -> Spec
viewLogicSpec env jwtCfg =
   with (return $ testApp env jwtCfg) $ do

    -- testcases for end point having both path variable and query parameter
    describe "GET /viewlogic/upc?pageKey=page_number" $ do
      it "should get all logic in the 0th page of corresponding upc" $do
          request methodGet
                "/viewlogic/1?pageKey=0"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          200 {matchBody = MatchBody viewLogicResponseMatcher}

      it "should get all logic in the 1st page of corresponding upc" $do
          request methodGet
                "/viewlogic/1?pageKey=1"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          200 {matchBody = MatchBody viewLogicResponseMatcher1}

      it "should get No data found on this page" $do
          request methodGet
                "/viewlogic/1?pageKey=2"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          404 {matchStatus = 404}

      it "should get incorrect upc" $do
          request methodGet
                "/viewlogic/1?pageKey=(-)2"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          400 {matchStatus = 400}
    
      it "should get page should be an Integer" $do
          request methodGet
                "/viewlogic/1?pageKey=abcd"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          400 {matchStatus = 400}
      
      it "should get page should be an Integer" $do
          request methodGet
                "/viewlogic/1?pageKey=$"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          400 {matchStatus = 400}



-- test cases for endpoint having only path variable no query parameter
    describe "GET /viewlogic/upc" $ do

      it "should get all logic of corresponding to upc" $do
          request methodGet
                "/viewlogic/0"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          200 {matchBody = MatchBody viewLogicResponseMatcher0}

      it "should get all logic of corresponding upc" $do
          request methodGet
                "/viewlogic/123"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          200 {matchBody = MatchBody viewLogicResponseMatcher123}
      
      it "should get all logic with both Store and Zone exceptions of corresponding upc" $do
          request methodGet
                "/viewlogic/121"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          200 {matchBody = MatchBody viewLogicResponseMatcher121}
      
      it "should get all logic with store exceptions of corresponding upc" $do
          request methodGet
                "/viewlogic/120"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          200 {matchBody = MatchBody viewLogicResponseMatcher121}

      it "could not parse upc since a negative number is given" $do
          request methodGet
                "/viewlogic/(-)1"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          400 {matchStatus = 400}   
 

      it "could not parse upc since the upc is given as special character" $do
          request methodGet
                "/viewlogic/@"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          400 {matchStatus = 400} 

      it "could not parse upc since the upc is given as decimal value" $do
          request methodGet
                "/viewlogic/1.9"
                [(hAuthorization,"Bearer "<>token)]
                ""
          `shouldRespondWith`
          400 {matchStatus = 400} 

   where
     viewLogicResponseMatcher _ body =
         case (decode body::Maybe (ApiResponse Response)) of
           Just val -> if (code val) == 200
                           then Nothing
                           else Just "Response body mismatch"
           Nothing  -> Just $ "Invalid Response Schema. Failed to Parse"++show body


     viewLogicResponseMatcher1 _ body =
         case (decode body::Maybe (ApiResponse Response)) of
           Just val -> if (code val) == 200
                           then Nothing
                           else Just "Response body mismatch"
           Nothing  -> Just $ "Invalid Response Schema. Failed to Parse"++show body


     viewLogicResponseMatcher123 _ body =
         case (decode body::Maybe (ApiResponse Response)) of
           Just val -> if (code val) == 200
                           then Nothing
                           else Just "Response body mismatch"
           Nothing  -> Just $ "Invalid Response Schema. Failed to Parse"++show body

     viewLogicResponseMatcher121 _ body =
         case (decode body::Maybe (ApiResponse Response)) of
           Just val -> if (code val) == 200
                           then Nothing
                           else Just "Response body mismatch"
           Nothing  -> Just $ "Invalid Response Schema. Failed to Parse"++show body
     
     viewLogicResponseMatcher120 _ body =
         case (decode body::Maybe (ApiResponse Response)) of
           Just val -> if (code val) == 200
                           then Nothing
                           else Just "Response body mismatch"
           Nothing  -> Just $ "Invalid Response Schema. Failed to Parse"++show body

     viewLogicResponseMatcher0 _ body =
         case (decode body::Maybe (ApiResponse Response)) of
           Just val -> if (code val) == 200
                           then Nothing
                           else Just "Response body mismatch"
           Nothing  -> Just $ "Invalid Response Schema. Failed to Parse"++show body
