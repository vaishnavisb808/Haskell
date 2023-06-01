{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


module          ShopperAPI.Services.Database.Postgres where


import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Services.Database.Types 

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
import           ShopperAPI.Services.Database.LoginDB
import           ShopperAPI.Services.Database.ManagerDB


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

getDbOps connectionPool = DbOps
    {
        initializeSchema = withConnection $ \conn -> initializePGSchema conn
     ,  orderConfirmation  = \orderid status-> withConnection $ \conn -> orderAcceptance orderid status conn
    }
    where withConnection = withResource connectionPool


-- | Script to initialize database
initializePGSchema :: Connection -> IO (Either DbError ())
initializePGSchema conn = catch
    (withTransaction conn $ do
        execute_
            conn
            -- "create table if not exists mgr (mgrid serial not null,\
            --     \ username varchar(512),email varchar(512),password varchar(512))\
            --     \ constraint mgr_pk PRIMARY KEY (mgrid));"
            
            "CREATE table if not exists login (login_id SERIAL NOT null ,\
                \email varchar NOT NULL,username varchar NOT NULL,\
                \password varchar NOT NULL,role varchar NOT NULL,\
                \CONSTRAINT login_pk PRIMARY KEY (login_id)) WITH (OIDS=FALSE);"

        return $ Right ()
    )
    handler
  where
    handler :: SomeException -> IO (Either DbError ())
    handler ex = return (Left $ DbError $ show ex)