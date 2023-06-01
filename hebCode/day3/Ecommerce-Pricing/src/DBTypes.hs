{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module DBTypes where

import Database.PostgreSQL.Simple
import Data.Map
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
import GHC.Int
import GHC.Generics
import Data.Time 
import Database.PostgreSQL.Simple.ToField (ToField(toField))
--import qualified Postgres as RuleType

-- | Data type for Logic 
data Logic = Logic
  { -- | List of store and zone exceptions unique for each logic id
    exceptions :: [Exception],
    -- | Details of rule_data column in rule table
    rule :: Rules,
    -- | Date from which the logic is effective
    effectiveFrom :: Day, 
    -- | Universal Product Code for each product
    upc :: Integer
  } deriving (Generic,Show)

instance ToJSON Logic
instance FromJSON Logic

-- | Data type for exceptions
data Exception = Exception
  { -- | rule_data for each exception
    exceptionRule :: Rules,
    exceptionData :: ExceptionData,
    -- | Specifying whether store or zone exception
    exceptionType :: ExceptionType
  }deriving (Generic,Show)

instance ToJSON Exception
instance FromJSON Exception

{-if exception type is store exception, it will contain store and zone number, if type is     zone exceotion then it will contain only zone number -}
data ExceptionData = StoreExceptionData ZoneNumber StoreNumber | ZoneExceptionData StoreNumber deriving (Generic,Show)
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
  | ZoneException  deriving (Generic,Show)
instance ToJSON ExceptionType
instance FromJSON ExceptionType

-- | Data type for Rules table
data Rules = Rules
  { -- | Details of rule_data column in rule table
    ruleData :: Rule,
    -- | Specifying the type of rule, in this case only markup
    ruleType :: RuleType
  }deriving (Generic,Show,Eq)

instance ToJSON Rules
instance FromJSON Rules

-- | Data type of  Rule when rule type is markup
data Rule = Rule
  { adjustmentMethod :: String,
    ignoreClearancePrice :: String,
    noMarkupIfOnAd :: String,
    markupBasisPoints :: Integer
  } deriving (Show, Eq, Generic)

instance ToJSON Rule
instance FromJSON Rule
instance ToRow Rule

data RuleType = Markup deriving (Show, Eq, Generic)  


instance ToJSON RuleType  where
  toJSON Markup = (String "Markup")

instance FromJSON RuleType where
  parseJSON (String "Markup") = pure Markup
instance ToField RuleType where
    toField Markup = toField $ show Markup 
