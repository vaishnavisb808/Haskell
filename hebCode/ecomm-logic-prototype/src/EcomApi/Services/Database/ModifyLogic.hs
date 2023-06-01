{-|
    Module          : ModifyLogic
    Description     : contains function for modifying rules when there is an updation
-}
{-# LANGUAGE OverloadedStrings #-}


module EcomApi.Services.Database.ModifyLogic (modifyLogic,selectupcData)where

import           EcomApi.Core.Types
import           EcomApi.Services.Database.Types    (DbError)

import           Data.Aeson
import           Data.Map
import           Data.Time
import           GHC.Generics
import           GHC.Int

import qualified Control.Exception.Base             as EB
import qualified Control.Monad.Catch                as C
import           Control.Monad.Except
import           Data.ByteString.Lazy.Internal      as B
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField (ToField (toField))


-- | function to Modify Rule when there is an updation

modifyLogic:: Connection -> Int -> Logic -> IO (Either DbError Int64)
modifyLogic conn logicId logic  = do
      let effectiveDate = effectiveFrom logic
      withTransaction conn $ do
              let ruledata = ruleData.rule $ logic
              let ruledata' = encode ruledata
              let ruletype =  ruleType.rule $ logic
              runExceptT $ do
                    selectAndDeleteExceptions logicId conn
                    insertRuleAndExceptions conn logicId (exceptions logic)
                    ruleId <- selectRuleById logicId conn
                    updateMainRule ruledata' ruletype ruleId conn
                    updateLogic effectiveDate logicId conn
                    updateUpc effectiveDate logicId conn

-- | Funtion for selecting effective_from using upc
selectupcData :: Integer -> Connection -> IO (Either String [(UTCTime,Int)])
selectupcData upc conn =
    C.catch ( do
    upcData <- (query conn                "SELECT effective_from, logic_id \
                                          \FROM upc WHERE upc =?"[upc] :: IO [(UTCTime,Int)] )
    return $Right upcData) handler


    where
        handler::C.SomeException->IO (Either String [(UTCTime,Int)])
        handler er = return (Left $  "Error while perfoming function selectupcData "++ show er )


-- | function to delete already existing rules
selectAndDeleteExceptions :: Int -> Connection -> ExceptT DbError IO Int64
selectAndDeleteExceptions logicId conn =ExceptT $ EB.try $ do
         ruleIds<- (query conn             "SELECT rule_id \
                                           \FROM Exceptions \
                                           \WHERE logic_id=?"[logicId]::IO[ Only Int])
         deleteExcep<- execute conn        "DELETE FROM Exceptions \
                                           \WHERE logic_id=?"[logicId]
         let ids=Prelude.map fromOnly ruleIds
         execute conn                      "DELETE FROM rules \
                                           \WHERE rule_id \
                                           \IN ?" $ Only (In ids)


-- | function to insert list of rules and exceptions
insertRuleAndExceptions:: Num a =>Connection -> Int -> [EcomApi.Core.Types.Exception] ->ExceptT DbError IO a
insertRuleAndExceptions conn logicId []= return 0
insertRuleAndExceptions conn logicId (x:ex)= do
        let ruledata = ruleData.exceptionRule $ x
        let ruleData = encode ruledata
        let ruletype =  ruleType.exceptionRule $ x
        ruleId<-insertExceptionRule ruleData ruletype conn
        insertExceptions ruleId logicId conn x
        insertRuleAndExceptions conn logicId ex


-- | function to insert new rules into rules table and returns rule_id
insertExceptionRule :: (ToField ruleData , ToField ruleType) => ruleData -> ruleType -> Connection -> ExceptT DbError IO Int
insertExceptionRule inputRuleData ruleType conn= ExceptT $ EB.try $ do
         ruleId <-query conn                 "INSERT INTO rules (rule_data, type) \
                                             \VALUES (?,?)\
                                             \RETURNING rule_id " (inputRuleData,ruleType)::IO[ Only Int]
         return $ fromOnly$ Prelude.head ruleId


-- | function to insert new rules into exceptions table
insertExceptions :: Int -> Int -> Connection -> EcomApi.Core.Types.Exception ->ExceptT DbError IO Int64
insertExceptions ruleId logicId conn exception= ExceptT $ EB.try $ do
        let excepType=show (exceptionType exception)
        let excepData= encode (exceptionData exception)
        execute conn              "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) \
                                             \VALUES (?,?,?,?)"(ruleId,logicId,excepType::String,excepData)


-- | Function to select rule_id from logic table using logic_id
selectRuleById :: Int-> Connection -> ExceptT DbError IO Int
selectRuleById logicId conn= ExceptT $ EB.try $ do
        fromOnly . Prelude.head <$> (query conn               "SELECT rule_id from logic \
                                            \WHERE logic_id =?"[logicId] :: IO [Only Int])


-- | Function to update rule for logic
updateMainRule ::(ToField ruleData , ToField ruleType) => ruleData -> ruleType -> Int -> Connection -> ExceptT DbError IO Int64
updateMainRule inputRuleData ruleType ruleId conn=ExceptT $ EB.try $
                         execute conn       "UPDATE rules \
                                            \SET rule_data =?,type = ? \
                                            \WHERE rule_id = ?" (inputRuleData,ruleType,ruleId)


-- | Function to update logic table
updateLogic :: UTCTime -> Int-> Connection -> ExceptT DbError IO Int64
updateLogic effectiveDate logicId conn = ExceptT $ EB.try $
                         execute conn      "UPDATE logic \
                                           \SET effective_from = ? \
                                           \WHERE logic.logic_id = ?" (effectiveDate,logicId)


-- | Function to update upc table
updateUpc :: UTCTime -> Int-> Connection -> ExceptT DbError IO Int64
updateUpc effectiveDate logicId conn = ExceptT $ EB.try $
                         execute conn     "UPDATE upc \
                                          \SET effective_from = ? \
                                          \WHERE upc.logic_id = ?"(effectiveDate,logicId)
