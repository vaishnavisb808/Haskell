{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Data.Time
import           Servant.Auth.Server
import           Test.Hspec

import           EcomApi.Core.Config.Types     (Configuration (..), Env (..))
import           EcomApi.Services.Logger.Types (LogLevel (..), LogType (..))
import           MockDatabase
import           ModifyLogicSpec
import           TransformersSpec
import           ViewLogicSpec                 (viewLogicSpec)


mockConfig = Configuration{
                            dbName="mockDb"
                          , dbUser="mockUser"
                          , dbPass="mockPass"
                          , dbHost="mockHost"
                          , logTypes=[Console]
                          , logLevel=DEBUG
                          , serverPort=8080
                          , authKey = key'
                          }


key' = "k0\\USH\\GS\\136[\\183\\233\\ETX`\\208\\&3\\150\\228\\DC3\\232\\141\\236\\187?\\168\\179\\SYN;\\149\\EOT@?\\213\\SYN\\243\\US\\253\\&9\\244m\\EM\\DLE\\224\\205y|C\\ETX\\144m\\164\\139o\\148\\226\\&6T\\234\\174R\\252\\&6I\\255M\\209\\217\\STX\'\\192\\SUB\\217P\\160\\SOH\\140\\149\\191h\\SI\\246x\\240\\236u\\vtp+\\RS\\153C\\168.M\\\"\\214\\\\8\\SYN\\155\\128\\199\\224\\255\\200`a\\202\\252\\207\\SI\\252+\\255\\149\\213\\255\\ETB\\248\\252\\vX\\159\\174p\\172\\&5@\\EOT\\f\\137\\218d\\196)k\\EM|\\167\\233v\\DC2\\157\\DC3\\173\\200Y\\174\\&6\\150\\201miM\\193\\194\\194\\159#>\\ETB1\\130^\\146\\211\\192\\211J\\238K^\\213\\140\\218J+TBw\\143hIM\\209\\252N\\215F\\130\\139\\220j\\217\\&5\\222\\179\\170\\190\\&1\\253R\\an\\181\\199\\254\\228\\206=\\201\\EM\\FS\\149\\204 5\\129-\\208c|\\140P\\CAN\\NUL\\185\\235E\\ENQxR\\254)\\163N\\224\\254f\\252\\150,\\150C\\248\\178\\232\\128\\220RR\\145\\ESC.\\229\\v\\229\\240"

main :: IO ()
main = do
    mockChanel <- newChan
    present_date <- liftIO $ getCurrentTime
    let key = fromSecret . encodeUtf8 . T.pack $ key'
    let jwtCfg = defaultJWTSettings key
    let mockDb = getMockDbOps
    let env = Env {
                    envDbOps=mockDb
                  , envConfig = mockConfig
                  , envLogChannel = mockChanel
                  }
    hspec $ viewLogicSpec env jwtCfg
    hspec $ modifyLogicSpec env jwtCfg present_date
    
    

