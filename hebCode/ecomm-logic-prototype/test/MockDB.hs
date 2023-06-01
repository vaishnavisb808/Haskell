{-# LANGUAGE OverloadedStrings #-}

module MockDB where

import EcomApi.Services.Database.ModifyLogic
import EcomApi.Services.Database.InsertLogic
import EcomApi.Services.Database.Types 
import EcomApi.Core.Types as CT

import Data.Time
import Data.Time.Clock
import Data.Time.Calendar.OrdinalDate
import           Servant
import           Servant.Auth.Server



getDbOps = DbOps insertLg modifyLg upcD viewLg


insertLg logic check = do
  let upc' = upc logic
  case upc' of
      1 -> return $ Right 1
      2 -> return $ Right 2
      _ -> return $ Left $ DbError "Error"
    

modifyLg logic_id logic = do
  let effectiveFrom' = (UTCTime (fromOrdinalDate 2022 22) 0)
  case logic_id of
      1 -> return $ Right 1
      _ -> return $ Left $ DbError "Error"


upcD upc = do 
    let x = read "2022-01-22":: Day
    let y = secondsToDiffTime 0
    case upc of
        1 -> return $ Right $ [(UTCTime x y, 1)] 
        2 -> return $ Right $ []
        _ -> return $ Left $ "Error"  
            
  
viewResp = CT.Logic [excepView] ruleView (UTCTime (fromOrdinalDate 2022 22) 0) 123
excepView = Exception ruleView excepD excepT
excepD = StoreExceptionData zoneNo storeNo 
storeNo = StoreNumber 23
zoneNo = ZoneNumber 45
excepT = CT.StoreException
ruleView = CT.Rule ruleD ruleT
ruleD = RuleData "bankersRounding"  "true" "true" 102
ruleT = Markup

viewLg upc = do
    case upc of  
        1 -> return $ Right $ [viewResp] 
        _ -> return $ Left $ DbError "Error"     



                                 

