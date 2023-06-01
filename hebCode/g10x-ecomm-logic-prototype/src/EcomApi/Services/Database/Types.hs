{-|
   Module : EcomApi.Services.Database.Types
   Description : Types for database operations
   Types for database operations
-}

module EcomApi.Services.Database.Types where


import qualified Control.Exception.Base            as EB (Exception)
import           Data.Time                         (UTCTime)
import           EcomApi.Api.Middleware.Auth.Types (AppData)
import           EcomApi.Core.Types                (Logic, UserInfo)
import           GHC.Int                           (Int64)

-- | Data type for database error messages
newtype DbError = DbError {
    dbError::String
    }deriving (Eq,Show)
instance EB.Exception DbError

-- | represents all possible operations performed by applcication
-- on any database. Used to abstract database associated functionality
-- from handlers
data DbOps = DbOps
    { insLogic   :: Logic -> [(UTCTime, Int)] -> IO (Either DbError Int64)
    , modLogic   :: Int -> Logic -> IO (Either DbError Int64)
    , getupcData :: Integer -> IO (Either DbError [(UTCTime,Int)])
    , getUserInfo :: String  -> IO (Either String UserInfo)
    , viewLogic  :: Integer ->  Int -> IO (Either DbError ([Logic],Maybe Int))
    , initializeSchema :: IO (Either DbError ())
    }
