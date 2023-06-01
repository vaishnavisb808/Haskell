{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

{-|
Module          : ModifyLogic
Description     : contains function for modifying rules when there is an updation
-}

module EcomApi.Services.Database.ModifyLogic where

import EcomApi.Core.Types
import EcomApi.Services.Database.Types

import Data.Time
import Data.Map
import GHC.Int
import Data.Aeson
import GHC.Generics

import Data.ByteString.Lazy.Internal as B
import Database.PostgreSQL.Simple
import           Control.Monad.Except
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import qualified Control.Monad.Catch as C
import qualified Control.Exception.Base as EB


-- | Funtion for selecting logic_id using upc

selectLogicid :: Int -> Connection -> IO Int
selectLogicid upc conn = do
    logicId <- (query conn                "SELECT logic_id\
                                          \FROM upc WHERE upc =?"[upc] :: IO [Only Int])
    let logicId' = fromOnly $ Prelude.head  logicId
    return logicId'  


-- | function to delete already existing rules
selectAndDeleteExceptions :: Int -> Connection -> (ExceptT DbError IO Int64)
selectAndDeleteExceptions logicId conn =ExceptT $ EB.try $ do
         ruleIds<- (query conn             "SELECT rule_id \
                                           \FROM Exceptions \
                                           \WHERE logic_id=?"[logicId]::IO[ Only Int])
         deleteExcep<- execute conn        "DELETE FROM Exceptions\
                                           \WHERE logic_id=?"[logicId]
         let ids=Prelude.map fromOnly ruleIds
         deleteRule<- execute conn         "DELETE FROM rules \
                                           \WHERE rule_id \
                                           \IN ?" $ Only (In ids)
         return deleteRule

-- | function to insert list of rules and exceptions 
insertRuleAndExceptions:: Num a =>Connection -> Int -> [EcomApi.Core.Types.Exception] ->ExceptT DbError IO a
insertRuleAndExceptions conn logicId []= return 0
insertRuleAndExceptions conn logicId (x:ex)= do
        let ruledata = ruleData.exceptionRuleData $ x
        let ruleData = encode ruledata
        let ruletype =  ruleType.exceptionRuleData $ x
        ruleId<-insertExceptionRule ruleData ruletype conn
        insertExceptions ruleId logicId conn x
        insertRuleAndExceptions conn logicId ex
         

-- | function to insert new rules into rules table and returns rule_id when there is updation
insertExceptionRule :: (ToField ruleData , ToField ruleType) => ruleData -> ruleType -> Connection -> (ExceptT DbError IO Int) 
insertExceptionRule inputRuleData ruleType conn= ExceptT $ EB.try $ do
         ruleId <-query conn                 "INSERT INTO rules (rule_data, type) \
                                             \VALUES (?,?)\
                                             \RETURNING rule_id " (inputRuleData,ruleType)::IO[ Only Int]
         return $ fromOnly$ Prelude.head ruleId
          


-- | function to insert new rules into exceptions table when there is updation
insertExceptions :: Int -> Int -> Connection -> EcomApi.Core.Types.Exception ->(ExceptT DbError IO Int64)
insertExceptions ruleId logicId conn exception= ExceptT $ EB.try $ do
        let excepType=show (exceptionType exception)
        let excepData= encode (exceptionData exception)
        insertR <-execute conn              "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) \
                                             \VALUES (?,?,?,?)"(ruleId,logicId,excepType::String,excepData)
        return insertR


-- | Function to select rule_id from logic table using logic_id to update main rule
selectRuleById :: Int-> Connection -> (ExceptT DbError IO Int)
selectRuleById logicId conn= ExceptT $ EB.try $ do
        ruleId <- (query conn               "SELECT rule_id from logic \
                                            \WHERE logic_id =?"[logicId] :: IO [Only Int])
        return $ fromOnly $ Prelude.head  ruleId


-- | Function to update main rule when there is updation 
updateMainRule ::(ToField ruleData , ToField ruleType) => ruleData -> ruleType -> Int -> Connection -> (ExceptT DbError IO Int64)
updateMainRule inputRuleData ruleType ruleId conn=ExceptT $ EB.try $ 
                         execute conn       "UPDATE rules \
                                            \SET rule_data =?,type = ? \
                                            \WHERE rule_id = ?" (inputRuleData,ruleType,ruleId) 


-- | Function to update logic table when there is updation

updateLogic :: UTCTime -> Int-> Connection -> (ExceptT DbError IO Int64)
updateLogic effectiveDate logicId conn = ExceptT $ EB.try $ 
                         execute conn      "UPDATE logic \
                                           \SET effective_from = ? \
                                           \WHERE logic.logic_id = ?" (effectiveDate,logicId)

-- | Function to update upc table when there is updation
updateUpc :: UTCTime -> Int-> Connection -> (ExceptT DbError IO Int64)
updateUpc effectiveDate logicId conn = ExceptT $ EB.try $ 
                         execute conn     "UPDATE upc \
                                          \SET effective_from = ? \
                                          \WHERE upc.logic_id = ?"(effectiveDate,logicId)
