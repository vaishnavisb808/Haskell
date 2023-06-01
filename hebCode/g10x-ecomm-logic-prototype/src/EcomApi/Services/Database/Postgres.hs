{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Postgres
    Description     : Integration module for all postgres database operations
    All funcitons for database operations are imported and rexported from postgres
    module and hence any module that needs to perform dpoperations needs to only
    import this module.
-}

module EcomApi.Services.Database.Postgres
    ( getDbOps
    , initConnectionPool
    ) where

import           Control.Exception                     (SomeException (SomeException),
                                                        catch, handle)
import           Data.Pool
import           Database.PostgreSQL.Simple            (Connection, close,
                                                        connect,
                                                        connectDatabase,
                                                        connectHost,
                                                        connectPassword,
                                                        connectUser,
                                                        defaultConnectInfo,
                                                        execute_,
                                                        withTransaction)
import           EcomApi.Core.Config.Types             (Configuration (..))
import qualified EcomApi.Services.Database.InsertLogic as InsLogic (insertLogic)
import qualified EcomApi.Services.Database.ModifyLogic as ModLogic (modifyLogic,
                                                                    selectupcData)
import           EcomApi.Services.Database.Types       (DbError (DbError),
                                                        DbOps (DbOps),
                                                        getUserInfo, getupcData,
                                                        initializeSchema,
                                                        insLogic, modLogic,
                                                        viewLogic)
import qualified EcomApi.Services.Database.ViewLogic   as ViewLogic (getLogicsByUpc)
import qualified EcomApi.Services.Database.GetAuthData as Auth (getAuthData)

import           GHC.Int                               (Int64)


-- | Creating new connection pool
initConnectionPool :: Configuration -> IO (Either String (Pool Connection))
initConnectionPool configVar = handle handler $ do
    -- establish a test connection to make sure the credentials provided are appropriate
    !testConnection <- initializeConnection configVar
    Right <$> createPool (initializeConnection configVar)
                         close
                         1 -- stripes
                         60 -- unused connections are kept open for a minute
                         10 -- max. 10 connections open per stripe
  where
    handler :: SomeException -> IO (Either String (Pool Connection))
    handler ex = return $ Left $ show ex


-- | Initializing database connection by using credentials from environment variable.
initializeConnection :: Configuration -> IO Connection
initializeConnection configVar = do
    connect defaultConnectInfo { connectDatabase = dbName configVar
                               , connectUser     = dbUser configVar
                               , connectPassword = dbPass configVar
                               , connectHost     = dbHost configVar
                               }

-- | returns a value of type DbOps loaded with all functions to perform postgres
-- db operations
getDbOps connectionPool = DbOps
    { insLogic         = \logic upcData -> withConnection
                             $ \conn -> InsLogic.insertLogic conn logic upcData
    , modLogic         = \logicId logic -> withConnection
                             $ \conn -> ModLogic.modifyLogic conn logicId logic
    , getupcData       = \upc -> withConnection
                             $ \conn -> ModLogic.selectupcData upc conn
    , getUserInfo      = \username ->
        withConnection $ \conn -> Auth.getAuthData username conn
    , viewLogic        = \upc lastid ->
                             withConnection $ \conn ->
                                 ViewLogic.getLogicsByUpc upc lastid conn
    , initializeSchema = withConnection $ \conn -> initializePGSchema conn
    }
    where withConnection = withResource connectionPool


-- | Script to initialize database
initializePGSchema :: Connection -> IO (Either DbError ())
initializePGSchema conn = catch
    (withTransaction conn $ do
        execute_
            conn
            "CREATE table if not exists Logic \
                    \(logic_id SERIAL not NULL,upc int8 NOT NULL,rule_id integer NOT NULL,\
                    \created_at timestamptz NOT null default CURRENT_TIMESTAMP,\
                    \effective_from timestamptz NOT NULL,\
                    \CONSTRAINT logic_pk PRIMARY KEY (logic_id))WITH (OIDS=FALSE);"

        execute_
            conn
            "CREATE TABLE IF NOT EXISTS Rules \
                    \(rule_id SERIAL not null, \
                    \created_at timestamptz not null default CURRENT_TIMESTAMP, \
                    \updated_at timestamptz not null default CURRENT_TIMESTAMP,\
                    \rule_data text not null, type varchar(512) not null,\
                    \CONSTRAINT Rules_pk PRIMARY KEY (rule_id))WITH (OIDS=FALSE);"

        execute_
            conn
            "CREATE table if not exists Exceptions\
                    \(exception_id SERIAL NOT null ,rule_id integer NOT NULL,\
                    \logic_id integer NOT NULL,type varchar NOT NULL,\
                    \exception_data TEXT NOT NULL,CONSTRAINT exceptions_pk\
                    \ PRIMARY KEY (exception_id)) WITH (OIDS=FALSE );"

        execute_
            conn
            "CREATE TABLE  if not exists upc \
                    \(upc int8 NOT NULL,logic_id integer NOT NULL,\
                    \effective_from TIMESTAMPTZ NOT NULL,\
                    \ CONSTRAINT upc_pk PRIMARY KEY (upc)) WITH (OIDS=FALSE );"

        execute_
            conn
            "ALTER TABLE logic ADD CONSTRAINT logic_fk0 \
                    \FOREIGN KEY (rule_id) REFERENCES rules(rule_id);"
        execute_
            conn
            "ALTER TABLE exceptions ADD CONSTRAINT exceptions_fk0 \
                    \FOREIGN KEY (rule_id) REFERENCES rules(rule_id);"
        execute_
            conn
            "ALTER TABLE exceptions ADD CONSTRAINT exceptions_fk1 \
                    \FOREIGN KEY (logic_id) REFERENCES logic(logic_id);"
        execute_
            conn
            "ALTER TABLE upc ADD CONSTRAINT upc_fk0 \
                    \FOREIGN KEY (logic_id) REFERENCES logic(logic_id);"

        execute_ conn "CREATE UNIQUE INDEX logic_index ON Logic(upc);"
        execute_ conn "CREATE UNIQUE INDEX logic_index ON Logic(rule_id);"
        execute_
            conn
            "CREATE UNIQUE INDEX exception_index ON Exceptions(logic_id);"
        execute_
            conn
            "CREATE UNIQUE INDEX exception_index ON Exceptions(rule_id);"

        return $ Right ()
    )
    handler
  where
    handler :: SomeException -> IO (Either DbError ())
    handler ex = return (Left $ DbError $ show ex)
