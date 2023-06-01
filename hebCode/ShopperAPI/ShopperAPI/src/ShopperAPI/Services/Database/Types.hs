{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module           ShopperAPI.Services.Database.Types where


import qualified Control.Exception.Base            as EB (Exception)
import           Data.Time                         (UTCTime)
import           GHC.Int                           (Int64)
import           Data.Aeson
import           GHC.Generics
import           Database.PostgreSQL.Simple






-- | Data type for database error messages
newtype DbError = DbError {
    dbError::String
    }deriving (Eq,Show)
instance EB.Exception DbError

data LoginInfo = LoginInfo
  { login_id   :: Integer
  , email      :: String
  , username   :: String
  , password   :: String
  , role       :: String
  } deriving (Eq, Show, Generic)
instance FromRow LoginInfo
instance ToRow LoginInfo

-- | represents all possible operations performed by applcication
-- on any database. Used to abstract database associated functionality
-- from handlers
data DbOps = DbOps
    { 
        initializeSchema :: IO (Either DbError ())
        ,orderConfirmation :: Int ->AcceptOrReject -> IO (Either String Int) 
    }

newtype OrderAccept = OrderAccept {
    status :: AcceptOrReject
}deriving Generic
instance ToField  OrderAccept

data AcceptOrReject = Accept | Reject |InProgress deriving ( Show, Generic)
instance ToField AcceptOrReject where
    toField Accept     = toField $ show Accept
    toField Reject     = toField $ show Reject
    toField InProgress = toField $ show InProgress