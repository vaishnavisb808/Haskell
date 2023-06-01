{-|
    Module : EcomApi.Services.Database.InsertLogic
    Description : Functions to insert one logic all associated data
                  to database
-}
{-# LANGUAGE OverloadedStrings #-}

module EcomApi.Services.Database.InsertLogic(insertLogic) where

import           Control.Exception                      (SomeException)
import           Control.Monad.Catch                    (handle)
import           Control.Monad.Except                   (ExceptT, runExceptT,
                                                         throwError)
import           Control.Monad.IO.Class                 (liftIO)
import           Data.Aeson                             (encode)
import qualified Data.Text                              as T
import           Data.Time                              (UTCTime)
import           Database.PostgreSQL.Simple             (Connection, Only,
                                                         execute, fromOnly,
                                                         query)
import           Database.PostgreSQL.Simple.Transaction (withTransaction)
import           EcomApi.Core.Types                     (Exception, Logic, Rule,
                                                         effectiveFrom,
                                                         exceptionData,
                                                         exceptionRule,
                                                         exceptionType,
                                                         exceptions, rule,
                                                         ruleData, ruleType,
                                                         upc)
import           EcomApi.Services.Database.Types        (DbError (DbError))
import           GHC.Int



-- | Function to insert logic into database
insertLogic:: Connection -> Logic ->[(UTCTime, Int)] -> IO (Either DbError Int64)
insertLogic conn logic upcData =do
  withTransaction conn $ runExceptT $ do
      ruleId' <- insertRule conn (rule logic) -- inserting values into rule table and returning ruleid
      logicId <- insertLogicData conn logic ruleId'  -- inserting data into logic table and returning logicid
      insertExceptionRule conn logicId (exceptions logic) -- inserting values into exception and rule tables recursively
      case upcData of
        [] -> insertUpc conn logicId (effectiveFrom logic) (upc logic)
        [(effFrm,logicid)] -> updateUpc conn (effectiveFrom logic) logicId (upc logic)




{-Inserting data into Rule table and returning rule id,
  inserting data into Exception table for the corresponding rule id recursively-}
insertExceptionRule:: Connection -> Int-> [Exception] -> ExceptT DbError IO ()
insertExceptionRule conn logicId [] = return ()
insertExceptionRule conn logicId (x:ex) = do
   ruleId<-insertRule conn (exceptionRule x)
   insertExceptions conn ruleId logicId x
   insertExceptionRule conn logicId ex


{- Inserting rule data and rule type into Rules table and returning rule,
   Handling exception inside catch block using ExceptT
-}
insertRule :: Connection -> Rule -> ExceptT DbError IO Int
insertRule conn rules = handle handler $ do
            let ruleType'= show (ruleType rules)
            let ruleData'= encode (ruleData rules)
            ruleId' <- liftIO
                ( query conn
                    "INSERT INTO public.rules(rule_data,type) \
                    \ VALUES (?,?) \
                    \ RETURNING rule_id"
                    (ruleData',ruleType':: String)
                  :: IO [Only Int]
                )
            let ruleId = fromOnly $ Prelude.head ruleId'
            return ruleId

  where
    handler :: SomeException -> ExceptT DbError IO Int
    handler er = throwError (DbError $ "error while perfoming function insertRule "
                                     ++ show er
                            )


{-| Inserting upc, rule id and effective from date into Logic table and returning logicId,
    Handling exception inside catch block using ExceptT
-}
insertLogicData :: Connection -> Logic -> Int -> ExceptT DbError IO Int
insertLogicData conn logic ruleId = handle handler $ do
    let upc' = upc logic
    let effectiveFrom' =  effectiveFrom logic
    logicId' <-liftIO
        ( query conn
                "INSERT INTO logic(upc, rule_id, effective_from) \
                \ VALUES (?,?,?) \
                \ RETURNING logic_id;"
                (upc', ruleId, effectiveFrom')
          :: IO [Only Int]
        )
    let logicId = fromOnly $ Prelude.head logicId'
    return logicId

  where
    handler::SomeException->ExceptT DbError IO Int
    handler er = throwError (DbError $ "error while perfoming function insertRule "
                                     ++ show er
                            )


{-| Inserting rule id, logic id, exception type and exception data into Exceptions,
  Handling exception inside catch block using ExceptT -}
insertExceptions:: Connection -> Int -> Int -> Exception ->ExceptT DbError IO Int64
insertExceptions conn ruleId logicId exception = handle handler $ do
    let exceType=show (exceptionType exception)
    let exceData= encode (exceptionData exception)
    liftIO (
           execute conn
                   "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) \
                   \ VALUES (?,?,?,?);"
                   (ruleId,logicId,exceType::String,exceData)
           )
  where
    handler :: SomeException->ExceptT DbError IO Int64
    handler er = throwError (DbError $ "error while perfoming function insertException "
                                     ++ show er
                            )


{-| Inserting upc, logic id and effective  from date into upc table
  Handling exception inside catch block using ExceptT -}
insertUpc :: Connection->Int->UTCTime->Integer->ExceptT DbError IO Int64
insertUpc conn logicId effectiveFrom upc= handle handler $
    liftIO (
            execute conn
                    " INSERT INTO upc (upc,logic_id, effective_from) \
                    \ VALUES (?,?,?);"
                    (upc,logicId, effectiveFrom)
            )

  where
      handler::SomeException->ExceptT DbError IO Int64
      handler er = throwError (DbError $ "error while perfoming function insertUpc "
                                       ++ show er )

-- | Function to update upc table
updateUpc :: Connection -> UTCTime -> Int->Integer -> ExceptT DbError IO Int64
updateUpc conn effectiveDate logicId upc= handle handler $
    liftIO (
            execute conn "UPDATE upc \
                         \ SET effective_from =?,logic_id =? \
                         \ WHERE upc=?"
                         (effectiveDate,logicId::Int,upc::Integer)
           )
  where
    handler::SomeException->ExceptT DbError IO Int64
    handler er = throwError (DbError $ "error while perfoming function updateUpc "
                                     ++ show er )
