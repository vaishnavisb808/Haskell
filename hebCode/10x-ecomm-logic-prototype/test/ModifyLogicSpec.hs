{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ModifyLogicSpec where

import           Data.Aeson
import           Servant
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           Network.HTTP.Types
import           Servant.Auth.Server
import           Control.Concurrent
import           GHC.Generics

import qualified Data.ByteString.Lazy.Internal     as BS
import qualified Network.Wai.Handler.Warp      as Warp

import           EcomApi.Api.Handler.ModifyLogic
import           EcomApi.Server
import           EcomApi.Core.Config.Types
import           EcomApi.Server as Server
import           EcomApi.Services.Logger.Types
import           MockDb
import qualified EcomApi.Api.Types as AT



-- | datatypes for testing
data Request' = Request'
 { futureLogic :: Logic'   -- ^ Logic to be modified
 , upc         :: Maybe Integer -- ^ Universal Product Code for each product
} deriving (Generic)
instance FromJSON Request'
instance ToJSON Request'

data Logic' = Logic'
   { storeExceptions :: Maybe [AT.StoreException]    -- ^ List of store exceptions of a logic id
   , zoneExceptions  :: Maybe [AT.ZoneException]     -- ^ List of zone exceptions of a logic id
   , rule            :: AT.Rule                -- ^ Rule details of a logic id
   , effective       :: Day            -- ^ Date from which the logic is effective
   }  deriving (Generic ,Show)
instance FromJSON  Logic'
instance ToJSON Logic'   

data ErrorResponse  = ErrorResponse  { 
  result :: Maybe String
  , error  :: Maybe String
  , code   :: Int
    }deriving (Show,Generic)
instance ToJSON ErrorResponse 
instance FromJSON ErrorResponse  

-- | Request body 
apiType1 = Request' logDHist (Just 1)
apiType2 = Request' logDHist (Just 2)
apiType4 = Request' logDHist Nothing
apiType6 = Request' logDHist6 (Just 1)
apiType7 = Request' logDHist6 (Just 1)
apiType8 = Request' logDHist (Just (-1))
apiType9 = Request' logDHist (Just 4126201901235)

logDHist = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "2022-01-30"::Day  )
logDHist6 = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "2022-01-25"::Day  )
logDHist7 = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "2022-02-30"::Day  )

storeExcep = AT.StoreException ruleD' 255 134
zoneExcep = AT.ZoneException ruleD' 134
ruleD' = AT.Rule "bankersRounding"  "true" "true" 102

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


modifyLogicSpec :: Env -> JWTSettings  -> Spec
modifyLogicSpec env jwtCfg =
   with (return $ testApp env jwtCfg) $ do

       describe "POST /modifylogic" $ do
          it "should update the rule when futurelogic is present" $ do
            (request 
                methodPost 
                "/modifylogic"  
                [(hContentType, "application/json" ),(hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
                (encode apiType1)) 
                `shouldRespondWith` 
                200 {matchBody = MatchBody postLogicResponseMatcher1}

          it "should create a new rule if futurelogic is not present" $ do
            (request 
                methodPost 
                "/modifylogic"  
                [(hContentType, "application/json" ),(hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
                (encode apiType2)) 
                `shouldRespondWith` 
                200 {matchBody = MatchBody postLogicResponseMatcher2}
          
            
          it "should respond with no request body error message" $ do
            (request 
                methodPost 
                "/modifylogic"  
                [(hContentType, "application/json" ),(hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
                "") 
                `shouldRespondWith` 
                400 {matchBody = MatchBody postLogicResponseMatcher3}
          
          it "to check when request body details are incorrect" $ do
            (request 
                methodPost 
                "/modifylogic"  
                [(hContentType, "application/json" ),(hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
                (encode apiType4)) 
                `shouldRespondWith` 
                400 {matchBody = MatchBody postLogicResponseMatcher4}

          it "to check for Authentication failure" $ do
            (request 
                methodPost 
                "/modifylogic"  
                [(hContentType, "application/json" )] 
                (encode apiType1)) 
                `shouldRespondWith` 
                401 {matchBody = MatchBody postLogicResponseMatcher5}   

          it "effective date is modified to present date" $ do
            (request
                  methodPost
                  "/modifylogic"  
                  [(hContentType, "application/json" ),
                  (hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")]
                  (encode apiType6 ))
                  `shouldRespondWith`
                  400 {matchBody = MatchBody postLogicResponseMatcher6 }

          it "invalid effective date is provided" $ do
            (request
                methodPost
                "/modifylogic"  
                [(hContentType, "application/json" ),
                (hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")]
                (encode apiType7 ))
                `shouldRespondWith`
                400 {matchBody = MatchBody postLogicResponseMatcher7 } 

          it "should display error message in Json format as UPC code should always be a Positive integer" $ do
            (request 
                methodPost 
                "/modifylogic"  
                [(hContentType, "application/json" ),(hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
                (encode apiType8)) 
                `shouldRespondWith` 
                400 {matchBody = MatchBody postLogicResponseMatcher8}   

          it " UPC is more than 12 digits" $ do
            (request 
             methodPost 
             "/modifylogic"  
             [(hContentType, "application/json" ),
             (hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
             (encode apiType9)) 
             `shouldRespondWith`
             400 {matchBody = MatchBody postLogicResponseMatcher10 }   
            

postLogicResponseMatcher1 _ body = case (eitherDecode body::Either String (AT.ApiResponse String)) of
                                   Right val -> case val of
                                     AT.ApiResponse "Inserted logic" Nothing 200  -> Nothing
                                     AT.ApiResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)


postLogicResponseMatcher2 _ body = case (eitherDecode body::Either String (AT.ApiResponse String)) of
                                   Right val -> case val of
                                     AT.ApiResponse "Inserted logic" Nothing 200  -> Nothing
                                     AT.ApiResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher3 _ body = case (eitherDecode body::Either String (ErrorResponse)) of
                                   Right val -> case val of
                                     ErrorResponse _ _ 400  -> Nothing
                                     ErrorResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)                                   

postLogicResponseMatcher4 _ body = case (eitherDecode body::Either String (ErrorResponse)) of
                                   Right val -> case val of
                                     ErrorResponse Nothing  (Just "Error in $.upc: parsing Integer failed, expected Number, but encountered Null") 400  -> Nothing
                                     ErrorResponse  _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher5 _ body = case (eitherDecode body::Either String (ErrorResponse)) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Unauthorized") 401  -> Nothing
                                     ErrorResponse  _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher6 _ body = case (eitherDecode body::Either String (ErrorResponse) ) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid date. Only future logic can be entered!") 400  -> Nothing
                                     ErrorResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just msg



postLogicResponseMatcher7 _ body = case (eitherDecode body::Either String (ErrorResponse ))  of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid date. Only future logic can be entered!") 400  -> Nothing
                                     ErrorResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)  

postLogicResponseMatcher8 _ body = case (eitherDecode body::Either String (ErrorResponse)) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid UPC . It should be a positive Integer with\
                                                              \ a max limit of 12 digits -1") 400  -> Nothing
                                     ErrorResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body) 

postLogicResponseMatcher10 _ body = case (eitherDecode body::Either String (ErrorResponse)) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid UPC . It should be a positive Integer with a max limit of 12 digits 4126201901235") 400  -> Nothing
                                     ErrorResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)                                   

