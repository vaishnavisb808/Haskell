{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ApiSpec where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Internal     as BS
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import qualified EcomApi.API.Types as AT
import EcomApi.API.Handler.ModifyLogic
import EcomApi.Server
import TestType

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Database.PostgreSQL.Simple
import Network.HTTP.Types
import           Servant.Auth.Server
import MockDB
import Control.Concurrent
import EcomApi.Core.Config.Types
import EcomApi.Server as Server
import EcomApi.Services.Logger.Types
import GHC.Generics

mockConfig = Configuration{
                            dbName="mockDb"
                          , dbUser="mockUser"
                          , dbPass="mockPass"
                          , dbHost="mockHost"
                          , logTypes=[Console]
                          , EcomApi.Core.Config.Types.logLevel=DEBUG
                          , authKey = "k0\\USH\\GS\\136[\\183\\233\\ETX`\\208\\&3\\150\\228\\DC3\\232\\141\\236\\187?\\168\\179\\SYN;\\149\\EOT@?\\213\\SYN\\243\\US\\253\\&9\\244m\\EM\\DLE\\224\\205y|C\\ETX\\144m\\164\\139o\\148\\226\\&6T\\234\\174R\\252\\&6I\\255M\\209\\217\\STX'\\192\\SUB\\217P\\160\\SOH\\140\\149\\191h\\SI\\246x\\240\\236u\\vtp+\\RS\\153C\\168.M\\\"\\214\\\\8\\SYN\\155\\128\\199\\224\\255\\200`a\\202\\252\\207\\SI\\252+\\255\\149\\213\\255\\ETB\\248\\252\\vX\\159\\174p\\172\\&5@\\EOT\\f\\137\\218d\\196)k\\EM|\\167\\233v\\DC2\\157\\DC3\\173\\200Y\\174\\&6\\150\\201miM\\193\\194\\194\\159#>\\ETB1\\130^\\146\\211\\192\\211J\\238K^\\213\\140\\218J+TBw\\143hIM\\209\\252N\\215F\\130\\139\\220j\\217\\&5\\222\\179\\170\\190\\&1\\253R\\an\\181\\199\\254\\228\\206=\\201\\EM\\FS\\149\\204 5\\129-\\208c|\\140P\\CAN\\NUL\\185\\235E\\ENQxR\\254)\\163N\\224\\254f\\252\\150,\\150C\\248\\178\\232\\128\\220RR\\145\\ESC.\\229\\v\\229\\240"
                          , serverPort=8080
                          }




testApp :: IO Application
testApp = do
    let mockDb = getDbOps
    ch <- newChan
    let env = Env {
                   envDbOps=mockDb
                   ,envConfig = mockConfig
                   , envLogChannel = ch
                  }
    return $ Server.app env

      
data Request' = Request'
 { futureLogic :: Logic'   -- ^ Logic to be modified
 , upc         :: Integer -- ^ Universal Product Code for each product
} deriving (Generic)
instance FromJSON Request'
instance ToJSON Request'


--resp = Response logD logD (Just [logDHist])  254 

--logD = Just $ Logic (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "2022-01-22"::Day )
data Logic' = Logic'
   { storeExceptions :: Maybe [StoreException]    -- ^ List of store exceptions of a logic id
   , zoneExceptions  :: Maybe [ZoneException]     -- ^ List of zone exceptions of a logic id
   , rule            :: Rule                -- ^ Rule details of a logic id
   , effective       :: Day            -- ^ Date from which the logic is effective
   }  deriving (Generic ,Show)
instance FromJSON  Logic'
instance ToJSON Logic'   

data ErrResponse = ErrResponse {
    result :: Maybe String
  , error  :: Maybe String
  , code   :: Int
    }deriving (Show,Generic)
instance FromJSON ErrResponse
instance ToJSON ErrResponse

apiType6 = Request' logDHist6 1
logDHist6 = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "2022-01-25"::Day  )

apiType7 = Request' logDHist7 1
logDHist7 = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "2022-20-01"::Day  )

apiType9 = Request' logDHist 12345678901

apiType10 = Request' logDHist 4126201901235

apiType = Request' logDHist 1

logDHist = Logic' (Just [storeExcep]) (Just [zoneExcep]) ruleD' (read "2022-01-30"::Day  )

storeExcep = StoreException ruleD' 255 134

zoneExcep = ZoneException ruleD' 134

ruleD' = Rule "bankersRounding"  "true" "true" 102


businessLogicSpec :: Spec
businessLogicSpec = do
  with (testApp) $ do
       describe "POST /modifylogic" $ do
          it "should create a new rule if futurelogic is not present" $ do
            (request 
             methodPost 
             "/modifylogic"  
             [(hContentType, "application/json" ),(hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
             (encode apiType)) 
             `shouldRespondWith` 
             200 {matchBody = MatchBody postLogicResponseMatcher2 }

       describe "POST /modifylogic" $ do
          it "effective date is modified to present date" $ do
            (request 
             methodPost 
             "/modifylogic"  
             [(hContentType, "application/json" ),
             (hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
             (encode apiType6 )) 
             `shouldRespondWith`
             500 {matchBody = MatchBody postLogicResponseMatcher6 }

       describe "POST /modifylogic" $ do
          it "invalid effective date is provided" $ do
            (request 
             methodPost 
             "/modifylogic"  
             [(hContentType, "application/json" ),
             (hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
             (encode apiType7 )) 
             `shouldRespondWith`
             500 {matchBody = MatchBody postLogicResponseMatcher7 }

       describe "POST /modifylogic" $ do
          it " UPC is more than 10 digits" $ do
            (request 
             methodPost 
             "/modifylogic"  
             [(hContentType, "application/json" ),
             (hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
             (encode apiType9)) 
             `shouldRespondWith`
             500 {matchBody = MatchBody postLogicResponseMatcher9 }

       describe "POST /modifylogic" $ do
          it " UPC is more than 12 digits" $ do
            (request 
             methodPost 
             "/modifylogic"  
             [(hContentType, "application/json" ),
             (hAuthorization, "Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXBwTmFtZSI6ImFrc2RmIiwiYXBwSWQiOiJ2aWphaSJ9fQ.f94gP2OnBd-CT0-yPTZ2a_etlp7PgxlHjvb7Jhs4cOLT-rgCaXwe7KWyCPj6asmowpC9HKi9a1IO5pHbUc203Q")] 
             (encode apiType10)) 
             `shouldRespondWith`
             400 {matchBody = MatchBody postLogicResponseMatcher10 }

postLogicResponseMatcher2 _ body = case (eitherDecode body::Either String (AT.ApiResponse String)) of
                                   Right val -> case val of
                                     AT.ApiResponse "Updated" Nothing 200  -> Nothing
                                     AT.ApiResponse _ _ _ -> Just ("Response body mismatch"++ show body)
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher6 _ body = case (eitherDecode body::Either String (ErrResponse) ) of
                                   Right val -> case val of
                                     ErrResponse Nothing (Just "Invalid date. Only future logic can be entered!") 500  -> Nothing
                                     ErrResponse _ _ _ -> Just "Response body mismatch"
                                   Left msg  -> Just(msg ++ show body)

postLogicResponseMatcher7 _ body = case (eitherDecode body::Either String (ErrResponse))  of
                                   Right val -> case val of
                                     ErrResponse Nothing (Just "Error in $.futureLogic: Invalid date") 500  -> Nothing
                                     ErrResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body)                                   
      
postLogicResponseMatcher9 _ body = case (eitherDecode body::Either String (ErrResponse)) of
                                   Right val -> case val of
                                     ErrResponse Nothing (Just"Updation failed, try again.") 500  -> Nothing
                                     ErrResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body) 

postLogicResponseMatcher10 _ body = case (eitherDecode body::Either String (ErrResponse)) of
                                   Right val -> case val of
                                     ErrResponse Nothing (Just"Invalid UPC . It should be a positive Integer \
                                                               \with a max limit of 12 digits 4126201901235") 400  -> Nothing
                                     ErrResponse _ _ _ -> Just ("Response body mismatch" ++ show body)
                                   Left msg  -> Just(msg ++ show body) 

                                   