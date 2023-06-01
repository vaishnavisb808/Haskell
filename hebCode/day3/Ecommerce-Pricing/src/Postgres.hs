{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Postgres where

import DBTypes    
import Database.PostgreSQL.Simple
import Data.Map
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
import GHC.Int
import GHC.Generics
import Data.Time
import Database.PostgreSQL.Simple.ToField (ToField(toField))



getConnection :: IO Connection
getConnection =do
    conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
    return conn

-- | parent function
modifyRule :: IO ()
modifyRule = do
  let id_logic = 13 ::Int
  rFile <- readFile "input.json"
  let d= eitherDecode (B.packChars rFile)::Either String Logic
  case d of
    Left e -> print e
    Right d-> do
      conn <-getConnection
      let effectiveDate = effectiveFrom d
      withTransaction conn $ do
          let ruledata = ruleData.rule $ d
          let inpruledata = encode ruledata
          let ruletype =  ruleType.rule $ d
          selectANDdeleteTable id_logic conn
          recuFunction conn id_logic (exceptions d)
          id_rule <- selectRuleById id_logic conn
          updateMainRule inpruledata ruletype id_rule conn
          updateLogicAndUpc effectiveDate id_logic conn


-- | function to delete already existing rules
selectANDdeleteTable :: Int -> Connection -> IO()
selectANDdeleteTable logicid conn = do
         ruleIds<- (query conn "select rule_id from Exceptions where logic_id=?"[logicid]::IO[ Only Int])
         deltetExcep<- execute conn "delete from Exceptions where logic_id=?"[logicid]
         let ids=Prelude.map fromOnly ruleIds
         deleteRule<- execute conn "delete from rules where rule_id IN ?" $ Only (In ids)
         print "Deleted old rules"

-- | function to insert list of rules and exceptions 
recuFunction ::  Num a =>  Connection -> Int -> [Exception] -> IO a
recuFunction conn logic_id []= return 1
recuFunction conn logic_id (x:ex)=do
    let ruledata = ruleData.exceptionRule$x
    let inpruledata = encode ruledata
    let ruletype =  ruleType.exceptionRule$ x
    rule_id<-insertRuleTables inpruledata ruletype conn
    insertExceptions rule_id logic_id conn x
    recuFunction conn logic_id ex

-- | function to insert new rules into rules table and returns rule_id
insertRuleTables :: (ToField a, ToField b) => a -> b -> Connection -> IO Int
insertRuleTables inpruledata ruletype conn= do
         ruleUpdate <- query conn "INSERT INTO rules (rule_data, type) VALUES (?,?)returning rule_id " (inpruledata,ruletype)::IO[ Only Int]
         let ruleid= fromOnly$ Prelude.head ruleUpdate
         return ruleid


-- | function to insert new rules into exceptions table
insertExceptions :: (ToField a, ToField b1, Num b2) =>a -> b1 -> Connection -> Exception -> IO b2
insertExceptions rule_id logic_id conn exception= do
    let exceType=show (exceptionType exception)
    let exceDta= encode (exceptionData exception)
    insertR <- execute conn "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) values (?,?,?,?)"(rule_id,logic_id,exceType::String,exceDta)
    return 0


-- | Function to select rule_id from logic table using logic_id
selectRuleById :: Int-> Connection -> IO Int
selectRuleById logicid conn= do
    idrule <- (query conn "SELECT rule_id from logic where logic_id =?"[logicid] :: IO [Only Int])
    let ruleid = fromOnly $ Prelude.head  idrule
    return ruleid


-- | Function to update main rule
updateMainRule :: (ToField a, ToField b, ToField c) =>a -> b -> c -> Connection -> IO ()
updateMainRule inpruledata ruletype ruleid conn= do
    updateRule <- execute conn "UPDATE rules SET rule_data =?,type = ? where rule_id = ?" (inpruledata,ruletype,ruleid) 
    print "Successfully updated main rule"



-- | Function to update logic table and upc table

updateLogicAndUpc :: Day -> Int-> Connection -> IO ()
updateLogicAndUpc effectiveDate logicid conn = do
    updateLogic <- execute conn "UPDATE logic SET effective_from = ? where logic.logic_id = ?" (effectiveDate,logicid)
    updateUpc <- execute conn "UPDATE upc SET effective_from = ? where upc.logic_id = ?"(effectiveDate,logicid)
    print "Successfully updated logic table"
    print "Successfully updated upc table"






