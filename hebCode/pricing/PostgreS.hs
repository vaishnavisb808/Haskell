{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Postgres where

import Types    
import Database.PostgreSQL.Simple
import Data.Map
import Data.Maybe
import Data.Aeson

import GHC.Int
import GHC.Generics
import Data.Time
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import qualified Control.Monad.Catch as C
import qualified Control.Exception.Base as B
import           Control.Monad.Except       (runExceptT, throwError)
import           Control.Monad.Trans.Except
import Control.Monad.IO.Class 


newtype DbError = DbError String deriving (Eq,Show)
instance B.Exception DbError

-- | function to Modify Rule
modifyRule:: Int -> Logic -> Connection -> IO (Either DbError Int64)
modifyRule logicId logic conn = do
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
              updateLogicAndUpc effectiveDate logicId conn
         


-- | function to delete already existing rules
selectAndDeleteExceptions :: Int -> Connection -> (ExceptT DbError IO Int64)
selectAndDeleteExceptions logicId conn = ExceptT $ B.try $ do
         ruleIds<-liftIO $ (query conn "select rule_id from Exceptions where logic_id=?"[logicId]::IO[ Only Int])
         deleteExcep<- execute conn "delete from Exceptions where logic_id=?"[logicId]
         let ids=Prelude.map fromOnly ruleIds
         deleteRule<- liftIO $ execute conn "delete from rules where rule_id IN ?" $ Only (In ids)
         return deleteRule

-- | function to insert list of rules and exceptions 
insertRuleAndExceptions:: Num a =>Connection -> Int -> [Exception] -> ExceptT DbError IO a
insertRuleAndExceptions conn logicId []= return 0
insertRuleAndExceptions conn logicId (x:ex)=do
        let ruledata = ruleData.exceptionRule $ x
        let ruleData = encode ruledata
        let ruletype =  ruleType.exceptionRule $ x
        ruleId<-insertRules ruleData ruletype conn
        insertExceptions ruleId logicId conn x
        insertRuleAndExceptions conn logicId ex
         


-- | function to insert new rules into rules table and returns rule_id
insertRules :: (ToField ruleData , ToField ruleType) => ruleData -> ruleType -> Connection -> (ExceptT DbError IO Int)
insertRules inputRuleData ruleType conn= ExceptT $ B.try $ do
         ruleId <-liftIO $ query conn "INSERT INTO rules (rule_data, type) VALUES (?,?)\
                               \returning rule_id " (inputRuleData,ruleType)::IO[ Only Int]
         let ruleId' = fromOnly$ Prelude.head ruleId
         return ruleId'


-- | function to insert new rules into exceptions table
insertExceptions :: Int -> Int -> Connection -> Exception ->(ExceptT DbError IO Int64)
insertExceptions ruleId logicId conn exception= do
    C.catch (do
        let excepType=show (exceptionType exception)
        let excepData= encode (exceptionData exception)
        insertR <- liftIO $ execute conn "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) \
                            \values (?,?,?,?)"(ruleId,logicId,excepType::String,excepData)
        return insertR
         ) handler where
         handler::B.SomeException->(ExceptT DbError IO Int64)
         handler ex = throwError (DbError $ "error while perfoming function insertExceptions "++ show ex )


-- | Function to select rule_id from logic table using logic_id
selectRuleById :: Int-> Connection -> (ExceptT DbError IO Int)
selectRuleById logicId conn= do
    C.catch (do
        ruleId <- liftIO $ (query conn "SELECT rule_id from logic where logic_id =?"[logicId] :: IO [Only Int])
        let ruleId' = fromOnly $ Prelude.head  ruleId
        return ruleId'
         ) handler where
         handler::B.SomeException->(ExceptT DbError IO Int)
         handler ex = throwError (DbError $ "error while perfoming function selectRuleById"++ show ex )
     


-- | Function to update main rule
updateMainRule ::(ToField ruleData , ToField ruleType) => ruleData -> ruleType -> Int -> Connection -> (ExceptT DbError IO Int64)
updateMainRule inputRuleData ruleType ruleId conn= do
    C.catch (do
        updateRule <-liftIO $ execute conn "UPDATE rules SET rule_data =?,type = ? \
                                \where rule_id = ?" (inputRuleData,ruleType,ruleId) 
        return  updateRule
         ) handler where
         handler::B.SomeException->(ExceptT DbError IO Int64)
         handler ex = throwError (DbError $ "error while perfoming function updateMainRule "++ show ex )



-- | Function to update logic table and upc table

updateLogicAndUpc :: Day -> Int-> Connection -> (ExceptT DbError IO Int64)
updateLogicAndUpc effectiveDate logicId conn = do
    C.catch (do 
        updateLogic <-liftIO $ execute conn "UPDATE logic SET effective_from = ? \
                                 \where logic.logic_id = ?" (effectiveDate,logicId)
        updateUpc <- liftIO $ execute conn "UPDATE upc SET effective_from = ? \
                               \where upc.logic_id = ?"(effectiveDate,logicId)
        return updateUpc
         )handler 
         where 
         handler::B.SomeException->(ExceptT DbError IO Int64)
         handler ex = throwError (DbError $"error while perfoming function updateLogicAndUpc"++ show ex )





