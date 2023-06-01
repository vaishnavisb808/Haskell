{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Postgre where

import Typ    
import Database.PostgreSQL.Simple
import Data.Map
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
import GHC.Int
import GHC.Generics
import Data.Time
import Database.PostgreSQL.Simple.ToField (ToField(toField))





-- | function to Modify Rule
modifyRule :: Int-> Logic ->Connection -> IO Int64
modifyRule logicId logic conn = do
      let effectiveDate = effectiveFrom logic
      withTransaction conn $ do
          let ruledata = ruleData.rule $ logic
          let ruleData = encode ruledata
          let ruletype =  ruleType.rule $ logic
          selectAndDeleteExceptions logicId conn
          insertRuleAndExceptions conn logicId (exceptions logic)
          ruleId <- selectRuleById logicId conn
          updateMainRule ruleData ruletype ruleId conn
          updateLogicAndUpc effectiveDate logicId conn


-- | function to delete already existing rules
selectAndDeleteExceptions :: Int -> Connection -> IO Int64
selectAndDeleteExceptions logicid conn = do
         ruleIds<- (query conn "select rule_id from Exceptions where logic_id=?"[logicid]::IO[ Only Int])
         deleteExcep<- execute conn "delete from Exceptions where logic_id=?"[logicid]
         let ids=Prelude.map fromOnly ruleIds
         deleteRule<- execute conn "delete from rules where rule_id IN ?" $ Only (In ids)
         return deleteExcep
         return deleteRule

-- | function to insert list of rules and exceptions 
insertRuleAndExceptions ::  Num a =>  Connection -> Int -> [Exception] -> IO a
insertRuleAndExceptions conn logic_id []= return 1
insertRuleAndExceptions conn logic_id (x:ex)=do
    let ruledata = ruleData.exceptionRule $ x
    let ruleData = encode ruledata
    let ruletype =  ruleType.exceptionRule $ x
    rule_id<-insertRules ruleData ruletype conn
    insertExceptions rule_id logic_id conn x
    insertRuleAndExceptions conn logic_id ex

-- | function to insert new rules into rules table and returns rule_id
insertRules :: ruleData -> ruleType -> Connection -> IO Int
insertRules inputRuleData ruleType conn= do
         ruleId <- query conn "INSERT INTO rules (rule_data, type) VALUES (?,?)\
                               \returning rule_id " (inputRuleData,ruleType)::IO[ Only Int]
         let ruleId' =fromOnly$ Prelude.head ruleId
         return ruleId'


-- | function to insert new rules into exceptions table
insertExceptions :: Int -> Int -> Connection -> Exception -> IO Int64
insertExceptions ruleId logicId conn exception= do
    let excepType=show (exceptionType exception)
    let excepData= encode (exceptionData exception)
    insertR <- execute conn "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) \
                            \values (?,?,?,?)"(ruleId,logicId,excepType::String,excepData)
    return insertR


-- | Function to select rule_id from logic table using logic_id
selectRuleById :: Int-> Connection -> IO Int
selectRuleById logicId conn= do
    ruleId <- (query conn "SELECT rule_id from logic where logic_id =?"[logicId] :: IO [Only Int])
    let ruleId' = fromOnly $ Prelude.head  ruleId
    return ruleId'
     


-- | Function to update main rule
updateMainRule :: ruleData -> ruleType -> Int -> Connection -> IO Int64
updateMainRule inputRuleData ruleType ruleId conn= do
    updateRule <- execute conn "UPDATE rules SET rule_data =?,type = ? \
                                \where rule_id = ?" (inputRuleData,ruleType,ruleId) 
    return updateRule



-- | Function to update logic table and upc table

updateLogicAndUpc :: Day -> Int-> Connection -> IO Int64
updateLogicAndUpc effectiveDate logicId conn = do
    updateLogic <- execute conn "UPDATE logic SET effective_from = ? \
                                 \where logic.logic_id = ?" (effectiveDate,logicId)

    updateUpc <- execute conn "UPDATE upc SET effective_from = ? \
                               \where upc.logic_id = ?"(effectiveDate,logicId)
    return updateLogic
    return updateUpc





