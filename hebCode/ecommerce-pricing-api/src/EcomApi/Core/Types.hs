{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module EcomApi.Core.Types where

import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import           Data.Aeson
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)


-- | Data type for Logic
data Logic = Logic
   { -- | List of store and zone exceptions unique for each logic id
     exceptions    :: [Exception]
    -- | Details of rule data
   , rule          :: Rules
    -- | Date from which the logic is effective
   , effectiveFrom :: UTCTime
    -- | Universal Product Code for each product
   , upc           :: Int
   }deriving (Show, Eq, Generic)
instance ToJSON Logic
instance FromJSON Logic

-- | Data type for exceptions
data Exception = Exception
   { -- | Rule data for each exception
     exceptionRuleData :: Rules
   , exceptionData     :: ExceptionData
    -- | Specifying whether store or zone exception
   , exceptionType     :: ExceptionType
   }deriving (Show, Eq, Generic)
instance ToJSON Exception
instance FromJSON Exception
instance FromField ExceptionType where
  fromField f mdata = case mdata of
        Just "StoreException" -> return StoreException 
        Just "ZoneException" -> return ZoneException 
        _      -> Prelude.error "error"


{-if exception type is store exception, it will contain store and zone number, if type is     zone exceotion then it will contain only zone number -}
data ExceptionData = StoreExceptionData {
    zoneNumber::ZoneNumber,
    storeNumber::StoreNumber
    }
     | ZoneExceptionData {
        storeNumber::StoreNumber}
         deriving (Show, Eq, Generic) 

instance ToJSON ExceptionData
instance FromJSON ExceptionData

newtype StoreNumber = StoreNumber Integer deriving (Show, Eq, Generic)
instance ToJSON StoreNumber
instance FromJSON StoreNumber

newtype ZoneNumber = ZoneNumber Integer deriving (Show, Eq, Generic)
instance ToJSON ZoneNumber
instance FromJSON ZoneNumber

data ExceptionType
  = StoreException
  | ZoneException
  deriving (Show, Eq, Generic)
instance ToJSON ExceptionType
instance FromJSON ExceptionType

-- | Data type for Rule
data Rules = Rules
   { -- | Details of rule data
     ruleData :: Rule
    -- | Specifying the type of rule, in this case only markup
   , ruleType :: RuleType
   }deriving (Show, Eq, Generic)
instance ToJSON Rules
instance FromJSON Rules

-- | Data type of  Rule when rule type is markup
data Rule = Rule
  { adjustmentMethod     :: String
  , ignoreClearancePrice :: String
  , noMarkupIfOnAd       :: String
  , markupBasisPoints    :: Integer
  } deriving (Show, Eq, Generic)
instance ToJSON Rule
instance FromJSON Rule

data RuleType = Markup deriving (Show, Eq, Generic)
instance ToJSON RuleType  where
  toJSON Markup = (String "Markup")
instance FromJSON RuleType where
  parseJSON (String "Markup") = pure Markup
instance ToField RuleType where
    toField Markup = toField $ show Markup
instance FromField RuleType where
  fromField f mdata = case mdata of
        Just "Markup" -> return Markup  
        _      -> Prelude.error "error"

data ApiResponse a = ApiResponse
    { result :: a
    , error  :: Maybe Int
    , code   :: Int
    }deriving (Show,Generic)

instance (ToJSON a)=>ToJSON (ApiResponse a)

