{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module          : Postgres
Description     : View the Logic of a particular upc specified through the endpoint
-}

module EcomApi.Services.Database.ViewLogic(getLogicsByUpc) where

import           EcomApi.Core.Types
import           EcomApi.Services.Database.Types

import           Control.Monad.Catch
import           Data.Aeson
import           Data.ByteString.Lazy.Internal        as B
import           Data.IntMap.Internal                 as Map
import           Data.Maybe                           as M
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           GHC.Generics

{-
get logic id, effective from, and Rules of the given UPC
Rules include rule_type and rule_data which is json string and it is parsed to Rule datatype.
-}
getRulesByUpc :: Connection -> Integer -> IO (Either String [(Int,UTCTime,Rule)])
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
                         ruleData ruledata = (fromJust ( decode $B.packChars ruledata::Maybe RuleData))

                         tologicIdRules (logic_id,effective_from,rule_data,ruletype) = (logic_id,effective_from,(Rule (ruleData rule_data) ruletype))

                         handler::SomeException->IO (Either String [(Int,UTCTime,Rule)])
                         handler er = return (Left $"SQL Error" ++ show er)


{-
Get the list of Exception corresponding to the logicids.
Each logic id may have store or/and zone exceptions.
These exceptions are obtained along with the particular logicid.
-}
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

                        ruleData ruledata = fromJust (decode $B.packChars ruledata:: Maybe RuleData)

                        toLogicIdExceptions (logic_id,rule_data,ruletype,exception_data,exceptiontype) = (logic_id,[(Exception (Rule (ruleData rule_data) ruletype) (exceptionData exception_data)exceptiontype)])

                        handler::SomeException->IO (Either String [(Int,[EcomApi.Core.Types.Exception])])
                        handler er = return (Left $"SQL Error" ++ show er)



{-|
Get Logics of particular upc
Includes past,present and future logics
-}
getLogicsByUpc :: Integer -> Connection -> IO (Either DbError [Logic])
getLogicsByUpc upc conn = do
     idrules <- getRulesByUpc conn upc
     case idrules of
             Left e -> return $ Left $ DbError $ show e
             Right rules -> do
                if (Prelude.null rules)
                    then return $ Right []
                    else do
                        let ids = Prelude.map (\(x,_,_)->x) rules
                        exception <- getExceptionByLogicId conn ids
                        case exception of
                            Left e -> return $ Left $ DbError $ show e
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

