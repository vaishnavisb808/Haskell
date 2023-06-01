{-|
   Module : EcomApi.Services.Database.Types
   Description : Types for database operations
   Types for database operations
-}

module EcomApi.Services.Database.Types where

import qualified Control.Exception.Base     as EB
import           Control.Monad.Trans.Except
import           Data.Time
import           Database.PostgreSQL.Simple
import           EcomApi.Core.Types
import           GHC.Int

-- | Data type for database error messages
newtype DbError = DbError {
    dbError::String
    }deriving (Eq,Show)
instance EB.Exception DbError

-- | represents all possible operations performed by applcication
-- on any database. Used to abstract database associated functionality
-- from handlers
data DbOps = DbOps
    { insLogic   :: Logic ->Bool-> IO (Either DbError Int64)
    , modLogic   :: Int -> Logic -> IO (Either DbError Int64)
    , getupcData :: Integer -> IO (Either DbError [(UTCTime,Int)])
    , viewLogic  :: Integer ->  Integer -> IO (Either DbError [Logic])
    }
