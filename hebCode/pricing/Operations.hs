{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


module Operations where
  
import Types as T
import Database.PostgreSQL.Simple
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B


main = do
  let id_logic = 13 ::Int
  rFile <- readFile "input.json"
  let d= eitherDecode (B.packChars rFile)::Either String Logic
  case d of
    Left e -> print e
    Right d-> do
      conn <-getConnection
      selectANDdeleteTable id_logic conn
      id_rule <- selectRuleById id_logic conn
      let ruledata = ruleData.rule $ d
      let inpruledata = encode ruledata
      let ruletype =  ruleType.rule $ d
      updateMainRule inpruledata ruletype id_rule conn
      recuFunction conn id_logic (exceptions d)
      updateLogicAndUpc d id_logic conn


selectANDdeleteTable logicid conn = do
         ruleIds<- (query conn "select rule_id from Exceptions where logic_id=?"[logicid]::IO[ Only Int])
         deltetExcep<- execute conn "delete from Exceptions where logic_id=?"[logicid]
         let ids=Prelude.map fromOnly ruleIds
         deleteRule<- execute conn "delete from rules where rule_id IN ?" $ Only (In ids)
         print deleteRule


selectRuleById logicid conn= do
    res  <- (query conn "SELECT rule_id from logic where logic_id =?"[logicid] :: IO [Only Int])
    let ruleid = fromOnly $ Prelude.head  res
    return ruleid


updateMainRule inpruledata ruletype ruleid conn= do
    updr <- execute conn "UPDATE rules SET rule_data =?,type = ? where rule_id = ?" (inpruledata,ruletype,ruleid) 
    print updr


recuFunction conn logic_id []= return 1
recuFunction conn logic_id (x:ex)=do
   let ruledata = ruleData.exceptionRule$x
   let inpruledata = encode ruledata
   let ruletype =  ruleType .exceptionRule$ x
   rule_id<-insertRuleTables inpruledata ruletype conn 
   insertExceptions rule_id logic_id conn x
   recuFunction conn logic_id ex
  

insertRuleTables inpruledata ruletype conn= do   
         ruleUpdate <- query conn "INSERT INTO rules (rule_data, type) VALUES (?,?)returning rule_id " (inpruledata,ruletype)::IO[ Only Int]
         let ruleid= fromOnly$ Prelude.head ruleUpdate
         return ruleid


insertExceptions rule_id logic_id conn exception= do
    let exceType=show (exceptionType exception)
    let exceDta= encode (exceptionData exception)
    insertR <- execute conn "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) values (?,?,?,?)"(rule_id,logic_id,exceType::String,exceDta)
    print insertR


updateLogicAndUpc effdate logicid conn = do
    let effectivedate = effectiveFrom effdate
    eff <- execute conn "UPDATE logic SET effective_from = ? where logic.logic_id = ?" (effectivedate,logicid)
    updup <- execute conn "UPDATE upc SET effective_from = ? where upc.logic_id = ?"(effectivedate,logicid)
    print eff
    print updup


getConnection =do
conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
return conn