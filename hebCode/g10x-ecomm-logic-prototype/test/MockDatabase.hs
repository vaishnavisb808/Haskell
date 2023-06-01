{-# LANGUAGE OverloadedStrings #-}

module MockDatabase where

import           Data.Time                             (Day)
import           Data.Time.Calendar.OrdinalDate        (fromOrdinalDate)
import           Data.Time.Clock                       (UTCTime (UTCTime),
                                                        secondsToDiffTime)
import           EcomApi.Core.Types                    as CT
import           EcomApi.Services.Database.InsertLogic
import           EcomApi.Services.Database.ModifyLogic
import           EcomApi.Services.Database.Types


getMockDbOps = DbOps
    { insLogic = mockInsertLogic
    , modLogic = mockModifyLogic
    , getupcData = mockGetUpcData
    , viewLogic = mockViewLogic
    , getUserInfo = mockGetUserInfo
    , initializeSchema = return $ Right  ()
    }

mockGetUserInfo uid = case uid of
                        "1" -> return $ Right $ UserInfo 1 "app_name" "pwd"
                        _   -> return $ Left $ "error"

mockInsertLogic logic check = do
  let upc' = upc logic
  case upc' of
      1 -> return $ Right 1
      2 -> return $ Right 2
      _ -> return $ Left $ DbError "Error"


mockModifyLogic logic_id logic = do
  let effectiveFrom' = UTCTime (fromOrdinalDate 2022 22) 0
  case logic_id of
      1 -> return $ Right 1
      _ -> return $ Left $ DbError "Error"

mockGetUpcData upc = do
    let x = read "2022-01-27":: Day
    let y = secondsToDiffTime 0
    case upc of
        0      -> return $ Right [(UTCTime x y, 0)]
        1      -> return $ Right [(UTCTime x y, 1)]
        2      -> return $ Right []
        120    -> return $ Right [(UTCTime x y, 120)]
        121    -> return $ Right [(UTCTime x y, 121)]
        123    -> return $ Right [(UTCTime x y, 123)]


        _ -> return $ Left $ DbError "Error"



ruleData' = RuleData "" "" "" 100

effective = read "2022-01-01 00:00:00 UTC"::UTCTime

rule' = Rule ruleData' Markup

zonNum = ZoneNumber 1000

storeNum = StoreNumber 1001

storeExcepData = StoreExceptionData zonNum storeNum

storeExecp = Exception rule' storeExcepData StoreException 

zoneExcepData = ZoneExceptionData zonNum

zoneExecp = Exception rule' zoneExcepData ZoneException 

exception = [storeExecp,zoneExecp]

storeExce = [storeExecp]

logic' = Logic [] rule' effective 1234

mockViewLogic:: Integer -> Int -> IO (Either DbError ([Logic],Maybe Int))
mockViewLogic upc page = do

                        case upc of
                            1 ->
                                case page of
                                    0 ->  return $ Right  ([logic'],Nothing)
                                    1 ->  return $ Right  ([logic' {exceptions = exception}],Nothing)
                                    2 -> return $ Right  ([],Nothing)
                            0   -> return $ Right  ([logic'],Nothing)
                            123 -> return $ Right  ([logic'],Nothing)
                            121 -> return $ Right  ([logic'{exceptions = exception}],Nothing)
                            120 -> return $ Right  ([logic'{exceptions = storeExce}],Nothing)

