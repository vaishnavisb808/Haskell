module EcomApi.Core.Handler.ModifyLogic where

import  EcomApi.Core.Types
import  EcomApi.Core.Config.Types
import   EcomApi.Core.Config.Config
import  EcomApi.Services.Database.Postgres
import  Servant
import  Database.PostgreSQL.Simple
import  Control.Monad.IO.Class
import  Database.PostgreSQL.Simple.ToField (ToField(toField))
import  EcomApi.Services.Database.ModifyLogic


getConnection :: IO Connection
getConnection = do
    conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "abcd"
                }
    return conn


-- | Handler function for modifyLogic endpoint when upc is present.
modifyLogic :: Logic ->Handler (ApiResponse String)
modifyLogic logic = do
    conn <- liftIO $ getConnection
    let upc' = upc logic
    logicId <- liftIO $ selectLogicid upc' conn
    variable<- liftIO $ modifyRule logicId logic conn
    case variable of
        Left e -> throwError err500
        Right y -> return $ ApiResponse "Updated" Nothing 200
