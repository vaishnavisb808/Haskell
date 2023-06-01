{-|
    Module : EcomApi.Services.Database.InsertLogic
    Description : Functions to insert one logic all associated data
                  to database
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module EcomApi.Services.Database.InsertLogic(insertLogic) where

import           Control.Monad.Catch                    hiding (Exception)
import           Control.Monad.Except                   (runExceptT, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.Text                              as T
import           Data.Time
import           Database.PostgreSQL.Simple             (Connection, Only,
                                                         execute, fromOnly,
                                                         query)
import           Database.PostgreSQL.Simple.Transaction
import           EcomApi.Core.Types
import           EcomApi.Services.Database.Types
import           GHC.Generics
import           GHC.Int



-- | Function to insert logic into database
insertLogic::Connection->Logic -> Bool -> IO (Either DbError Int64)
insertLogic conn logic check =do
  withTransaction conn $ do
    runExceptT $ do
      ruleId' <- insertRule conn (rule logic) -- inserting values into rule table and returning ruleid
      logicId <- insertLogicData conn logic ruleId'  -- inserting data into logic table and returning logicid
      insertExceptionRule conn logicId (exceptions logic) -- inserting values into exception and rule tables recursively
      if True == check
        then do
          insertUpc conn logicId (effectiveFrom logic) (upc logic)-- inserting values into upc table
        else
          updateUpc conn (effectiveFrom logic) logicId (upc logic)



{-Inserting data into Rule table and returning rule id,
  inserting data into Exception table for the corresponding rule id recursively-}
insertExceptionRule:: Connection->Int->[Exception]->ExceptT DbError IO ()
insertExceptionRule conn logicId [] = return ()
insertExceptionRule conn logicId (x:ex) = do
   ruleId<-insertRule conn (exceptionRule x)
   insertExceptions conn ruleId logicId x
   insertExceptionRule conn logicId ex


{- Inserting rule data and rule type into Rules table and returning rule,
   Handling exception inside catch block using ExcpetT -}
insertRule :: Connection->Rule->ExceptT DbError IO Int
insertRule conn rules =
  catch ( do
    let ruleType'= show (ruleType rules)
    let ruleData'= encode (ruleData rules)
    ruleId' <- liftIO ( query conn
                    "INSERT INTO public.rules \
                        \ (rule_data,type) \
                        \ VALUES \
                        \ (?,?) \
                        \ RETURNING rule_id" (ruleData',ruleType':: String) :: IO [Only Int] )
    let ruleId = fromOnly $ Prelude.head ruleId'
    return ruleId ) handler

    where
        handler::SomeException->ExceptT DbError IO Int
        handler er = throwError (DbError $ "error while perfoming function insertRule "++ show er )


{-| Inserting upc, rule id and effective from date into Logic table and returning logicId,
  Handling exception inside catch block using ExcpetT -}
insertLogicData :: Connection->Logic->Int->ExceptT DbError IO Int
insertLogicData conn logic ruleId = do
  catch ( do
    let upc' = upc logic
    let effectiveFrom' =  effectiveFrom logic
    logicId' <-liftIO (query conn
                    "INSERT INTO logic \
                        \ (upc, rule_id, effective_from) \
                        \ VALUES \
                        \ (?,?,?) \
                        \ RETURNING logic_id" (upc', ruleId, effectiveFrom') :: IO [Only Int] )
    let logicId = fromOnly $ Prelude.head logicId'
    return logicId ) handler

    where
        handler::SomeException->ExceptT DbError IO Int
        handler er = throwError (DbError $ "error while perfoming function insertRule "++ show er )


{-| Inserting rule id, logic id, exception type and exception data into Exceptions,
  Handling exception inside catch block using ExcpetT -}
insertExceptions:: Connection->Int->Int->Exception->ExceptT DbError IO Int64
insertExceptions conn ruleId logicId exception = do
    catch ( do
        let exceType=show (exceptionType exception)
        let exceData= encode (exceptionData exception)
        liftIO ( execute conn
                      "INSERT INTO exceptions \
                      \ (rule_id,logic_id,type,exception_data) \
                      \ VALUES \
                      \ (?,?,?,?)"(ruleId,logicId,exceType::String,exceData))
        ) handler

  where
    handler :: SomeException->ExceptT DbError IO Int64
    handler er = throwError (DbError $ "error while perfoming function insertException "++ show er )


{-| Inserting upc, logic id and effective  from date into upc table
  Handling exception inside catch block using ExcpetT -}
insertUpc :: Connection->Int->UTCTime->Integer->ExceptT DbError IO Int64
insertUpc conn logicId effectiveFrom upc= do
  catch (
      liftIO ( execute conn
                       " INSERT INTO upc \
                       \ (upc,logic_id, effective_from) \
                       \ VALUES \
                       \ (?,?,?);" (upc,logicId, effectiveFrom))
    ) handler

  where
      handler::SomeException->ExceptT DbError IO Int64
      handler er = throwError (DbError $ "error while perfoming function insertUpc "++ show er )

-- | Function to update upc table
updateUpc :: Connection -> UTCTime -> Int->Integer -> ExceptT DbError IO Int64
updateUpc conn effectiveDate logicId upc= do
    catch (
            liftIO (   execute conn     "UPDATE upc \
                                          \ SET effective_from =?,logic_id =? \
                                          \ WHERE upc=?"(effectiveDate,logicId::Int,upc::Integer))
      ) handler
    where
        handler::SomeException->ExceptT DbError IO Int64
        handler er = throwError (DbError $ "error while perfoming function updateUpc "++ show er )
