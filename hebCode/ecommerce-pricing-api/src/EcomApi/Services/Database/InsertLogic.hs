{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module EcomApi.Services.Database.InsertLogic where

import           EcomApi.Core.Types
import           Data.Aeson
import qualified Data.Text                              as T
import           Database.PostgreSQL.Simple.Transaction
import           GHC.Generics
import           GHC.Int
import           Data.Time
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple             (Only
                                                         , Connection
                                                         , execute
                                                         , fromOnly
                                                         , query)

import           Control.Monad.Except       (runExceptT, throwError)
import           Control.Monad.Trans.Except
import Control.Monad.Catch hiding (Exception)

data DbError = DbError String deriving (Eq,Show)

insertLogicData::Connection->Logic -> IO (Either DbError Int64)                           
insertLogicData conn logic=do 
    withTransaction conn $ do 
        runExceptT $ do
            ruleId' <- insertRule conn (rule logic) -- inserting values into rule table and returning ruleid
            logicId <- insertLogic conn logic ruleId'  -- inserting data into logic table and returning logicid
            insertExceptionRule conn logicId (exceptions logic) -- inserting values into exception and rule tables recursively
            insertUpc conn logicId (effectiveFrom logic) (upc logic)-- inserting values into upc table
        
{-Inserting data into Rules table and returning rule id, 
  inserting data into Exception table for the corresponding rule id recursively-}

insertExceptionRule:: Connection->Int->[Exception]->(ExceptT DbError IO ())
insertExceptionRule conn logicId []= return ()
insertExceptionRule conn logicId (x:ex)=do
   ruleId<-insertRule conn (exceptionRuleData x)
   insertExceptions conn ruleId logicId x
   insertExceptionRule conn logicId ex

-- | Inserting rule data and rule type into Rules table and returning rule id
insertRule:: Connection->Rules->(ExceptT DbError IO Int)
insertRule conn rules=
  catch ( do
    let ruleType'=show (ruleType rules)
    let ruleData'=encode (ruleData rules)
    ruleId' <-liftIO ( query conn 
                    "INSERT INTO public.rules \
                        \ (rule_data,type) \
                        \ values \
                        \ (?,?) \
                        \ RETURNING rule_id"(ruleData',ruleType'::String) :: IO [Only Int])
    let ruleId = fromOnly $ Prelude.head ruleId'
    return ruleId ) handler
    where
        handler::SomeException->ExceptT DbError IO Int
        handler er = throwError (DbError $ "error while perfoming function insertRule "++ show er )

-- |Inserting upc, rule id and effective from date into Logic table and returning logicId
insertLogic:: Connection->Logic->Int->(ExceptT DbError IO Int)
insertLogic conn logic ruleId =do
  catch ( do
    let upc' = upc logic
    let effectiveFrom' =  effectiveFrom logic
    logicId' <-liftIO (query conn 
                    "INSERT INTO logic \
                    \ (upc, rule_id, effective_from) \
                    \ values \
                    \ (?,?,?) \
                    \ RETURNING logic_id"(upc', ruleId, effectiveFrom') :: IO [Only Int])
    let logicId = fromOnly $ Prelude.head logicId'
    return logicId ) handler

    where
        handler::SomeException->ExceptT DbError IO Int
        handler er = throwError (DbError $ "error while perfoming function insertRule "++ show er )

-- | Inserting rule id, logic id, exception type and exception data into Exceptions 
insertExceptions:: Connection->Int->Int->Exception->(ExceptT DbError IO Int64)
insertExceptions conn ruleId logicId exception= do
  catch ( do
    let exceType=show (exceptionType exception)
    let exceData= encode (exceptionData exception)
    affectedRow <-liftIO ( execute conn 
                      "INSERT INTO exceptions \
                      \ (rule_id,logic_id,type,exception_data) \
                      \ values \
                      \ (?,?,?,?)"(ruleId,logicId,exceType::String,exceData))
    return affectedRow) handler                   
    where
        handler::SomeException->ExceptT DbError IO Int64
        handler er = throwError (DbError $ "error while perfoming function insertException "++ show er )

-- | Inserting upc, logic id and effective  from date into upc table
insertUpc:: Connection->Int->UTCTime->Int->(ExceptT DbError IO Int64)
insertUpc conn logicId effective_from upc= do
  catch ( do
    affectedRow <-liftIO ( execute conn 
                       "INSERT INTO upc \
                       \ (upc,logic_id, effective_from) \
                       \ VALUES \
                       \ (?,?,?);"(upc,logicId, effective_from))
    return affectedRow)handler              

    where
        handler::SomeException->ExceptT DbError IO Int64
        handler er = throwError (DbError $ "error while perfoming function insertUpc "++ show er )
    