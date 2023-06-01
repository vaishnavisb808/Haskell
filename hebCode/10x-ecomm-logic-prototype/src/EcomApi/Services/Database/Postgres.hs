{-|
Module          : Postgres
Description     : Integration module for all postgres database operations
All funcitons for database operations are imported and rexported from postgres
module and hence any module that needs to perform dpoperations needs to only
import this module.
-}

module EcomApi.Services.Database.Postgres(getDbOps,initConnectionPool) where

import           Data.Pool
import           Database.PostgreSQL.Simple
import           EcomApi.Core.Config.Types
import qualified EcomApi.Services.Database.InsertLogic as InsLogic (insertLogic)
import qualified EcomApi.Services.Database.ModifyLogic as ModLogic (modifyLogic,
                                                                    selectupcData)
import           EcomApi.Services.Database.Types
import qualified EcomApi.Services.Database.ViewLogic   as ViewLogic (getLogicsByUpc)
import           GHC.Int
import           System.Environment



-- | Creating new connection pool
initConnectionPool ::Configuration -> IO (Pool Connection)
initConnectionPool configVar = do
  createPool (initializeConnection configVar)
             close
             1 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe


-- | Initializing database connection by using credentials from environment variable.
initializeConnection :: Configuration -> IO Connection
initializeConnection configVar = do
    connect defaultConnectInfo{
      connectDatabase= dbName configVar,
      connectUser = dbUser configVar,
      connectPassword = dbPass configVar,
      connectHost = dbHost configVar
    }

-- | returns a value of type DbOps loaded with all functions to perform postgres
-- db operations
getDbOps connectionPool = DbOps
    { insLogic = \logic check -> withConnection $ \conn -> InsLogic.insertLogic conn logic check
    , modLogic = \logicId logic -> withConnection $ \conn -> ModLogic.modifyLogic conn logicId logic
    , getupcData = \upc ->  withConnection $ \conn -> ModLogic.selectupcData upc conn
    , viewLogic = \upc page -> withConnection $ \conn -> ViewLogic.getLogicsByUpc upc page conn 
     

    
    }
  where
    withConnection  = withResource connectionPool
