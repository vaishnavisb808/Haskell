{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ModifyLogicSpec where

import           Control.Concurrent
import           Data.Aeson
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           GHC.Generics
import           Network.HTTP.Types
import           Servant
import           Servant.Auth.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import qualified Data.ByteString.Lazy.Internal     as BS
import qualified Network.Wai.Handler.Warp          as Warp

import           EcomApi.Api.Handler.ModifyLogic
import           EcomApi.Api.Middleware.Auth.Auth
import           EcomApi.Api.Middleware.Auth.Types
import qualified EcomApi.Api.Types                 as AT
import           EcomApi.Core.Config.Types
import           EcomApi.Server
import           EcomApi.Services.Logger.Types
import           MockDatabase


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
  result  :: Maybe String
  , error :: Maybe String
  , code  :: Int
    }deriving (Show,Generic)
instance ToJSON ErrorResponse
instance FromJSON ErrorResponse

-- | Request body
apiType1 = Request' logDHist (Just 1)
apiType2 = Request' logDHist (Just 2)
apiType4 = Request' logDHist Nothing
apiType8 = Request' logDHist (Just (-1))
apiType9 = Request' logDHist (Just 4126201901235)

logDHist = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "21999-02-10"::Day  )


storeExcep = AT.StoreException ruleD' 255 134
zoneExcep = AT.ZoneException ruleD' 134
ruleD' = AT.Rule "bankersRounding"  "true" "true" 102


token = "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q"

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


modifyLogicSpec :: Env -> JWTSettings -> UTCTime  -> Spec
modifyLogicSpec env jwtCfg presentDate = do
  let storeExcep = AT.StoreException ruleD' 255 134
  let zoneExcep = AT.ZoneException ruleD' 134
  let ruleD' = AT.Rule "bankersRounding"  "true" "true" 102
  let logDHist6 = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (utctDay presentDate )
  let apiType6 = Request' logDHist6 (Just 3)
  let apiType7 = Request' logDHist6 (Just 1)
  with (return $ testApp env jwtCfg) $

       describe "POST /modifylogic" $ do
     it "should update the rule when futurelogic is present" $ do
       request
           methodPost
           "/modifylogic"
           [(hContentType, "application/json" ),(hAuthorization, token)]
           (encode apiType1)
           `shouldRespondWith`
           200 {matchBody = MatchBody postLogicResponseMatcher1}

     it "should create a new rule if futurelogic is not present" $ do
       request
           methodPost
           "/modifylogic"
           [(hContentType, "application/json" ),(hAuthorization, token)]
           (encode apiType2)
           `shouldRespondWith`
           200 {matchBody = MatchBody postLogicResponseMatcher2}


     it "should respond with no request body error message" $ do
       request
           methodPost
           "/modifylogic"
           [(hContentType, "application/json" ),(hAuthorization, token)]
           ""
           `shouldRespondWith`
           400 {matchBody = MatchBody postLogicResponseMatcher3}

     it "to check when request body details are incorrect" $ do
       request
           methodPost
           "/modifylogic"
           [(hContentType, "application/json" ),(hAuthorization, token)]
           (encode apiType4)
           `shouldRespondWith`
           400 {matchBody = MatchBody postLogicResponseMatcher4}

     it "to check for Authentication failure" $ do
       request
           methodPost
           "/modifylogic"
           [(hContentType, "application/json" )]
           (encode apiType1)
           `shouldRespondWith`
           401 {matchBody = MatchBody postLogicResponseMatcher5}

     it "effective date is modified to present date" $ do
       request
             methodPost
             "/modifylogic"
             [(hContentType, "application/json" ),
             (hAuthorization, token)]
             (encode apiType6 )
             `shouldRespondWith`
             400 {matchBody = MatchBody postLogicResponseMatcher6 }

     it "invalid effective date is provided" $ do
       request
           methodPost
           "/modifylogic"
           [(hContentType, "application/json" ),
           (hAuthorization, token)]
           (encode apiType7 )
           `shouldRespondWith`
           400 {matchBody = MatchBody postLogicResponseMatcher7 }

     it "should display error message in Json format as UPC code should always be a Positive integer" $ do
       request
           methodPost
           "/modifylogic"
           [(hContentType, "application/json" ),(hAuthorization, token)]
           (encode apiType8)
           `shouldRespondWith`
           400 {matchBody = MatchBody postLogicResponseMatcher8}

     it " UPC is more than 12 digits" $ do
       request
        methodPost
        "/modifylogic"
        [(hContentType, "application/json" ),
        (hAuthorization, token)]
        (encode apiType9)
        `shouldRespondWith`
        400 {matchBody = MatchBody postLogicResponseMatcher10 }


postLogicResponseMatcher1 _ body = case (eitherDecode body::Either String (AT.ApiResponse String)) of
                                   Right val -> case val of
                                     AT.ApiResponse "Inserted logic" Nothing 200  -> Nothing
                                     AT.ApiResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)


postLogicResponseMatcher2 _ body = case (eitherDecode body::Either String (AT.ApiResponse String)) of
                                   Right val -> case val of
                                     AT.ApiResponse "Inserted logic" Nothing 200  -> Nothing
                                     AT.ApiResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher3 _ body = case (eitherDecode body::Either String ErrorResponse) of
                                   Right val -> case val of
                                     ErrorResponse _ _ 400  -> Nothing
                                     ErrorResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher4 _ body = case (eitherDecode body::Either String ErrorResponse) of
                                   Right val -> case val of
                                     ErrorResponse Nothing  (Just "Error in $.upc: parsing Integer failed, expected Number, but encountered Null") 400  -> Nothing
                                     ErrorResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher5 _ body = case (eitherDecode body::Either String ErrorResponse) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Unauthorized") 401  -> Nothing
                                     ErrorResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher6 _ body = case (eitherDecode body::Either String ErrorResponse ) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid date. Only future logic can be entered!") 400  -> Nothing
                                     ErrorResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just msg



postLogicResponseMatcher7 _ body = case (eitherDecode body::Either String ErrorResponse)  of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid date. Only future logic can be entered!") 400  -> Nothing
                                     ErrorResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher8 _ body = case (eitherDecode body::Either String ErrorResponse) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid UPC . It should be a positive Integer with a max limit of 12 digits -1") 400  -> Nothing
                                     ErrorResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher10 _ body = case (eitherDecode body::Either String ErrorResponse) of
                                   Right val -> case val of
                                     ErrorResponse Nothing (Just "Invalid UPC . It should be a positive Integer with a max limit of 12 digits 4126201901235") 400  -> Nothing
                                     ErrorResponse {} -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)

