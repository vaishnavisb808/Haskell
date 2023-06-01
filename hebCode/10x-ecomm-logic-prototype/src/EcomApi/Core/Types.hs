{-|
    Module : EcomApi.Core.Types
    Description : Types used throughout the application for processing and
                  for database manipulation
-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module EcomApi.Core.Types where

import           Data.Aeson
import           Data.Time                            (UTCTime)
import           Database.PostgreSQL.Simple.FromField (FromField (fromField))
import           Database.PostgreSQL.Simple.ToField   (ToField (toField))
import           GHC.Generics                         (Generic)


-- | Data type for Logic
data Logic = Logic
   { exceptions    :: [Exception]  -- ^ List of store and zone exceptions unique for each logic id
   , rule          :: Rule  -- ^ Details of rule data
   , effectiveFrom :: UTCTime  -- ^ Date from which the logic is effective
   , upc           :: Integer  -- ^ Universal Product Code for each product
   }deriving (Show, Eq, Generic)
instance ToJSON Logic
instance FromJSON Logic

-- | Data type for exceptions
data Exception = Exception
   { exceptionRule :: Rule   -- ^ Rule data for each exception
   , exceptionData :: ExceptionData  -- ^ Exception data for each exception
   , exceptionType :: ExceptionType  -- ^ Specifying whether store or zone exception
   }deriving (Show, Eq, Generic)
instance ToJSON Exception
instance FromJSON Exception


{-if exception type is store exception, it will contain store and zone number,
     if type is zone exceotion then it will contain only zone number -}
data ExceptionData = StoreExceptionData
                        { zoneNumber  :: ZoneNumber
                        , storeNumber :: StoreNumber
                        }
                   | ZoneExceptionData
                        { zoneNumber :: ZoneNumber
                        } deriving (Show, Eq, Generic)
instance ToJSON ExceptionData
instance FromJSON ExceptionData

-- | New type wrapper store number
newtype StoreNumber = StoreNumber
   {unStoreNumber::Integer  -- ^ Store number
   } deriving (Show, Eq, Generic)
instance ToJSON StoreNumber
instance FromJSON StoreNumber

-- | New type wrapper store number
newtype ZoneNumber = ZoneNumber
   {unZoneNumber::Integer  -- ^ Zone number
   } deriving (Show, Eq, Generic)
instance ToJSON ZoneNumber
instance FromJSON ZoneNumber

-- | Data type for Exceptiontype
data ExceptionType
  = StoreException
  | ZoneException
  deriving (Show, Eq, Generic, Ord)
instance ToJSON ExceptionType
instance FromJSON ExceptionType
instance FromField ExceptionType where
  fromField f mdata = case mdata of
        Just "StoreException" -> return StoreException
        Just "ZoneException"  -> return ZoneException
        _                     -> Prelude.error "error"

-- | Data type for Rule
data Rule = Rule
   { ruleData :: RuleData  -- ^ Details of rule data
   , ruleType :: RuleType  -- ^ Specifying the type of rule, in this case only markup
   }deriving (Show, Eq, Generic)
instance ToJSON Rule
instance FromJSON Rule

-- | Data type of  Rule when rule type is markup
data RuleData = RuleData
   { adjustmentMethod     :: String
   , ignoreClearancePrice :: String
   , noMarkupIfOnAd       :: String
   , markupBasisPoints    :: Integer
   } deriving (Show, Eq, Generic)
instance ToJSON RuleData
instance FromJSON RuleData

-- | Specifies the type of Rule, currently has only one value MARKUP
data RuleType = Markup deriving (Show, Eq, Generic)
instance ToJSON RuleType  where
  toJSON Markup = String "Markup"
instance FromJSON RuleType where
  parseJSON (String "Markup") = pure Markup
  parseJSON _                 = fail "Invalid value for ruleType"
instance ToField RuleType where
    toField Markup = toField $ show Markup
instance FromField RuleType where
  fromField f mdata = case mdata of
        Just "Markup" -> return Markup
        _             -> Prelude.error "error"
