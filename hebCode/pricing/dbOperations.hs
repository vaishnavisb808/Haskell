{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DeriveGeneric     #-}


import Data.Map
import Data.Maybe
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Internal as B
import Database.PostgreSQL.Simple

data Rules = Rules {
    rulesdata :: Rule,
    rtype::String,
    effective :: String
} deriving (Generic, Show)

instance FromJSON Rules
instance ToJSON Rules

data Rule = Rule
    { adjustmentMethod :: String
      ,ignoreClearancePrice :: String
      ,noMarkupIfOnAd:: String
      ,markupBasisPoints :: Int
    } deriving (Generic,Show) 

instance FromJSON Rule
instance ToJSON  Rule

main = do
  let id_logic = 13 ::Int
  rFile <- readFile "roughinp.json"
  let d= decode (B.packChars rFile)::Maybe Rules
  if(Prelude.null d)
       then putStrLn "empty file"
    else do
       let actualOp = (\ (Just x) -> x)d
       id_rule <- selectRuleById id_logic
       updateRule actualOp id_rule
       updateLogicAndUpc actualOp id_logic
       selectANDdeleteTable id_logic
    --    print id_rule

updateLogicAndUpc :: Rules -> Int -> IO()   
updateLogicAndUpc effdate logicid = do
    conn <-getConnection
    let effectivedate = effective effdate
    eff <- execute conn "UPDATE logic SET effective_from = ? where logic.logic_id = ?" (effectivedate,logicid)
    updup <- execute conn "UPDATE upc SET effective_from = ? where upc.logic_id = ?"(effectivedate,logicid)
    print eff
    print updup

selectRuleById :: Int -> IO Int
selectRuleById logicid = do
    conn <-getConnection
    res  <- (query conn "SELECT rule_id from logic where logic_id =?"[logicid] :: IO [Only Int])
    let ruleid = fromOnly $ Prelude.head  res
    return ruleid

updateRule :: Rules -> Int -> IO ()
updateRule rules ruleid = do
    conn <-getConnection
    let ruledata = rulesdata rules
    let inpruledata = encode ruledata
    let ruletype = rtype rules
    updr <- execute conn "UPDATE rules SET rule_data =?,type = ? where rule_id = ?" (inpruledata,ruletype,ruleid)
    print updr


selectANDdeleteTable logicid= do
         
         conn<-getConnection
         ruleIds<- (query conn "select rule_id from Exceptions where logic_id=?"[logicid]::IO[ Only Int])
         deltetExcep<- execute conn "delete from Exceptions where logic_id=?"[logicid]
         let ids=Prelude.map fromOnly ruleIds
         deleteRule<- execute conn "delete from rules where rule_id IN ?" $ Only (In ids)
         uRows<-updateTables logicid
         print uRows

updateTables logicid= do
         conn <-getConnection
         ruleUpdate <- query_ conn "INSERT INTO public.Rules (rule_data, type) VALUES ('afdghdh','rule')returning rule_id " ::IO[ Only Int]
         let ruleid= fromOnly$ Prelude.head ruleUpdate
         excepUpdate <- execute conn  "INSERT INTO exceptions (rule_id,logic_id,type,exception_data) VALUES (?,?,'fsfdufiej','kishdfujsfs');" (ruleid,logicid)
         return excepUpdate

getConnection =do
conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
return conn





-- parseDay :: String -> Day
-- parseDay s = readTime defaultTimeLocale "%-m%-d%Y" s

-- inPutFun=do
--     let effectiveFrom =parseDay "2022-04-02"
--     let rules = Rules (Rule "a" "a" "a" 9) Markup
--     let store=StoreNumber 6
--     let exceData= ZoneExceptionData store
--     let exception=Exception rules exceData StoreException
--     let logic = Logic [exception,exception] rules effectiveFrom  123
--     return logic  