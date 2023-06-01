{-# LANGUAGE OverloadedStrings #-}

module MockDB where

import EcomApi.Services.Database.Types

import EcomApi.API.Types as A
import Data.Time.Clock
import Data.Time.Calendar.OrdinalDate
import           Servant
import           Servant.Auth.Server

import EcomApi.Core.Types as CT
import EcomApi.Services.Database.ModifyLogic
import EcomApi.Services.Database.InsertLogic
import Data.Time
-- import Data.MBox
import Data.Attoparsec.ByteString.Char8
import GHCi.Utils







apiResp = ApiResponse "Updated" Nothing 200
apiResp1 = ApiResponse "Created" Nothing 200



getDbOps = DbOps insertLg modifyLg upcD viewLg

insertLg logic check = do
  --let upc' = upc logic
  let effectiveFrom' =  (UTCTime (fromOrdinalDate 2022 22) 0)
  if True == check
    then return $ Right 1
    else return $ Left $ DbError "Error"


modifyLg logic_id logic = do
  let effectiveFrom' = (UTCTime (fromOrdinalDate 2022 22) 0)
  if (logic_id /= 0)
    then return $ Right 1
    else return $ Left $ DbError "Error"


upcD upc = do 
    let x = read "2022-01-22"::Day 
    let y = secondsToDiffTime 0
    if (upc /= 0)
        then
            return $ Right $ [(UTCTime x y, 234)]
     else 
        return $ Left $ "Error"


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
    if (upc /= 0)  
        then return $ Right $ [viewResp] 
    else
        return $ Left $ DbError "Error"




resp = Response logD logD (Just [logDHist])  123
logD = Just $ A.Logic (Just [storeExcep]) (Just[zoneExcep]) ruleD' (UTCTime (fromOrdinalDate 2022 22) 0)
logDHist = A.Logic (Just [storeExcep]) (Just[zoneExcep]) ruleD' (UTCTime (fromOrdinalDate 2022 22) 0)
storeExcep =  A.StoreException ruleD' 23 45
zoneExcep =  A.ZoneException ruleD' 45
ruleD' = A.Rule "bankersRounding"  "true" "true" 102


respon = ApiResponse resp Nothing 200