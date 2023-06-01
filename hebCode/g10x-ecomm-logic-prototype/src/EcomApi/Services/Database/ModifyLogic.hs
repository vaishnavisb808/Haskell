{-|
    Module          : ModifyLogic
    Description     : contains function for modifying rules when there is an updation
-}
{-# LANGUAGE OverloadedStrings #-}


module EcomApi.Services.Database.ModifyLogic (modifyLogic,selectupcData)where

import           EcomApi.Core.Types              (Exception, Logic, Rule,
                                                  RuleData, RuleType,
                                                  effectiveFrom, exceptionData,
                                                  exceptionRule, exceptionType,
                                                  exceptions, rule, ruleData,
                                                  ruleType, upc)

import           EcomApi.Services.Database.Types (DbError (DbError))

import           Control.Exception               (SomeException)
import           Control.Monad.Catch             (handle)
import           Control.Monad.Except            (ExceptT, runExceptT,
                                                  throwError)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Aeson                      (encode)
import           Data.ByteString.Lazy.Internal   as B (ByteString)
import           Data.Time                       (UTCTime)
import           Database.PostgreSQL.Simple      (Connection, In (In),
                                                  Only (Only), execute,
                                                  fromOnly, query,
                                                  withTransaction)
import           GHC.Int                         (Int64)

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
selectupcData :: Integer -> Connection -> IO (Either DbError [(UTCTime,Int)])
selectupcData upc conn = handle handler $
    Right <$> (
                query conn
                      " SELECT effective_from, logic_id \
                      \ FROM upc WHERE upc =?;"[upc]
                :: IO [(UTCTime,Int)]
               )

  where
    handler :: SomeException->IO (Either DbError [(UTCTime,Int)])
    handler er = return (Left $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er )


-- | function to delete already existing rules
selectAndDeleteExceptions :: Int -> Connection -> ExceptT DbError IO Int64
selectAndDeleteExceptions logicId conn = handle handler $ do
         ruleIds<- liftIO
                       ( query conn
                             " SELECT rule_id \
                             \ FROM Exceptions \
                             \ WHERE logic_id=?;"
                             [logicId]
                         ::IO[ Only Int]
                       )
         deleteExcep <- liftIO $ execute conn
                                " DELETE FROM Exceptions \
                                \ WHERE logic_id=?;"[logicId]
         let ids=Prelude.map fromOnly ruleIds
         liftIO $ execute conn
                          " DELETE FROM rules \
                          \ WHERE rule_id \
                          \ IN ?" $ Only (In ids)
  where
    handler :: SomeException -> ExceptT DbError IO Int64
    handler er = throwError $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er


-- | function to insert list of rules and exceptions
insertRuleAndExceptions :: Connection
                        -> Int
                        -> [EcomApi.Core.Types.Exception]
                        -> ExceptT DbError IO Int
insertRuleAndExceptions conn logicId []= return 0
insertRuleAndExceptions conn logicId (x:ex)= do
        let ruledata = ruleData.exceptionRule $ x
        let ruleData = encode ruledata
        let ruletype =  ruleType.exceptionRule $ x
        ruleId<-insertExceptionRule ruleData ruletype conn
        insertExceptions ruleId logicId conn x
        insertRuleAndExceptions conn logicId ex


-- | function to insert new rules into rules table and returns rule_id
insertExceptionRule ::  ByteString  -> RuleType -> Connection -> ExceptT DbError IO Int
insertExceptionRule inputRuleData ruleType conn = handle handler $ do
    ruleId <- liftIO (
                     query conn
                           " INSERT INTO rules (rule_data, type) \
                           \ VALUES (?,?) \
                           \ RETURNING rule_id;" (inputRuleData,ruleType)
                     ::IO[ Only Int]
                     )
    return $ fromOnly $ Prelude.head ruleId
  where
    handler :: SomeException -> ExceptT DbError IO Int
    handler er = throwError $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er


-- | function to insert new rules into exceptions table
insertExceptions :: Int -> Int -> Connection -> EcomApi.Core.Types.Exception ->ExceptT DbError IO Int64
insertExceptions ruleId logicId conn exception= handle handler $ do
        let excepType=show (exceptionType exception)
        let excepData= encode (exceptionData exception)
        liftIO $ execute conn
                         " INSERT INTO exceptions (rule_id,logic_id,type,exception_data) \
                         \ VALUES (?,?,?,?);"(ruleId,logicId,excepType::String,excepData)
  where
    handler :: SomeException -> ExceptT DbError IO Int64
    handler er = throwError $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er


-- | Function to select rule_id from logic table using logic_id
selectRuleById :: Int-> Connection -> ExceptT DbError IO Int
selectRuleById logicId conn= handle handler $
    fromOnly . Prelude.head <$> liftIO ( query conn
                                               " SELECT rule_id from logic \
                                               \ WHERE logic_id =?"[logicId]
                                         :: IO [Only Int]
                                       )
  where
    handler :: SomeException -> ExceptT DbError IO Int
    handler er = throwError $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er


-- | Function to update rule for logic
updateMainRule :: ByteString  -> RuleType -> Int -> Connection -> ExceptT DbError IO Int64
updateMainRule inputRuleData ruleType ruleId conn= handle handler $
    liftIO $ execute conn " UPDATE rules \
                          \ SET rule_data =?,type = ? \
                          \ WHERE rule_id = ?" (inputRuleData,ruleType,ruleId)
  where
    handler :: SomeException -> ExceptT DbError IO Int64
    handler er = throwError $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er


-- | Function to update logic table
updateLogic :: UTCTime -> Int-> Connection -> ExceptT DbError IO Int64
updateLogic effectiveDate logicId conn = handle handler $
                         liftIO $ execute conn      "UPDATE logic \
                                           \SET effective_from = ? \
                                           \WHERE logic.logic_id = ?" (effectiveDate,logicId)
  where
    handler :: SomeException -> ExceptT DbError IO Int64
    handler er = throwError $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er


-- | Function to update upc table
updateUpc :: UTCTime -> Int-> Connection -> ExceptT DbError IO Int64
updateUpc effectiveDate logicId conn = handle handler $
                         liftIO $ execute conn     "UPDATE upc \
                                          \SET effective_from = ? \
                                          \WHERE upc.logic_id = ?"(effectiveDate,logicId)
  where
    handler :: SomeException -> ExceptT DbError IO Int64
    handler er = throwError $ DbError $ "Error while perfoming function selectupcData "
                                        ++ show er
