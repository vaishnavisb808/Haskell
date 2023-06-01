module Testing where


import Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy.Internal as B
import Typ
import Postgre


getConnection :: IO Connection
getConnection =do
    conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "abcd"
                }
    return conn    



main = do
    let logicId = 789 ::Int
    rFile <- readFile "input.json"
    let d= eitherDecode (B.packChars rFile)::Either String Logic
    case d of
        Left e -> print e
        Right d-> do
            conn <-getConnection
            modifyRule logicId d conn
