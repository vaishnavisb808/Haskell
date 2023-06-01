{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

{-|
Module          : Postgres
Description     : View the Logic of a particular upc specified through the endpoint
-}

module EcomApi.Services.Database.Postgres(getLogicsByUpc,insertLogicData,modifyRule) where
import EcomApi.Core.Types
import EcomApi.Services.Database.Types
import EcomApi.Services.Database.InsertLogic(insertLogicData)
import EcomApi.Services.Database.ModifyLogic

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple
import Data.Time
import GHC.Int
import GHC.Generics
import Data.IntMap.Internal as Map
import Database.PostgreSQL.Simple.FromField
import Data.Aeson
import Data.ByteString.Lazy.Internal as B
import Data.Maybe as M
import Control.Monad.Catch
import qualified Control.Exception.Base as EB
import           Control.Monad.Except       (runExceptT, throwError)
import           Control.Monad.Trans.Except
import Control.Monad.IO.Class 



-- | get logic id, effective from, and Rules of the given UPC 
-- Rules include rule_type and rule_data which is json string and it is parsed to Rule datatype.
getRulesByUpc :: Connection -> Int -> IO (Either String [(Int,UTCTime,Rules)])
getRulesByUpc conn upc = do
                    catch(do
                        logicidruleslist <- query conn
                          "SELECT p1.logic_id ,p1.effective_from ,p2.rule_data,p2.type \
                          \FROM public.Logic p1 \
                          \INNER JOIN public.Rules p2 \
                          \ON p1.rule_id = p2.rule_id \
                          \WHERE p1.upc=?" [upc] :: IO [(Int,UTCTime,String,RuleType)]
                        return $ Right $ fmap tologicIdRules logicidruleslist
                     ) handler
                     where
                         ruleData ruledata = (fromJust ( decode $B.packChars ruledata::Maybe Rule))

                         tologicIdRules (logic_id,effective_from,rule_data,ruletype) = (logic_id,effective_from,(Rules (ruleData rule_data) ruletype)) 

                         handler::SomeException->IO (Either String [(Int,UTCTime,Rules)])
                         handler er = return (Left $"SQL Error" ++ show er)


-- | Get the list of Exception corresponding to the logicids.
-- Each logic id may have store or/and zone exceptions.
-- These exceptions are obtained along with the particular logicid.
getExceptionByLogicId :: Connection -> [Int]-> IO ( Either String [(Int,[EcomApi.Core.Types.Exception])])
getExceptionByLogicId conn logicIds = do
                catch (do
                    logicidexceplist <- query conn
                        "SELECT logic_id,p2.rule_data,p2.type,p1.exception_data,p1.type \
                        \FROM public.Exceptions p1 \
                        \INNER JOIN public.Rules p2 \
                        \ON p1.rule_id = p2.rule_id \
                        \WHERE p1.logic_id IN ?;" $Only (In logicIds)

                    return $Right $ fmap toLogicIdExceptions logicidexceplist ) handler

                    where
                        exceptionData excep =  fromJust (decode $B.packChars excep::Maybe ExceptionData)
                        
                        ruleData ruledata = fromJust (decode $B.packChars ruledata:: Maybe Rule)

                        toLogicIdExceptions (logic_id,rule_data,ruletype,exception_data,exceptiontype) = (logic_id,[(Exception (Rules (ruleData rule_data) ruletype) (exceptionData exception_data)exceptiontype)])  

                        handler::SomeException->IO (Either String [(Int,[EcomApi.Core.Types.Exception])])
                        handler er = return (Left $"SQL Error" ++ show er)



-- | Get Logics of particular upc
-- Includes past,present and future logics
getLogicsByUpc :: Int -> Connection -> IO (Either String [Logic])
getLogicsByUpc upc conn = do
     idrules <- getRulesByUpc conn upc
     case idrules of
             Left e -> return $ Left e
             Right rules -> do
                let ids = Prelude.map (\(x,_,_)->x) rules
                exception <- getExceptionByLogicId conn ids
                case exception of
                    Left e -> return $ Left e
                    Right excep -> do
                        let map = Map.fromListWith (++) excep
                        return $ Right $ createLogicList rules map upc


{-
Removed duplicate values of logicId
Got the final list with exception rule effective from and upc
-}
createLogicList [] _ _ =  []
createLogicList ((id,effectiveFrom,rule):xs) duplicateMap upc= do
    let exception  = Map.lookup id duplicateMap
    let pureMap = Map.delete id duplicateMap
    case exception of
        Just exception -> do
            Logic exception rule effectiveFrom upc : createLogicList xs pureMap upc
        Nothing -> do createLogicList xs duplicateMap upc

-- | function to Modify Rule when there is an updation
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
                    updateLogic effectiveDate logicId conn
                    updateUpc effectiveDate logicId conn
