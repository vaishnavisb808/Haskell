{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DeriveGeneric     #-}
module DbpersistentLayer where
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple (ToRow)


-- data Dbpersistent =  Dbpersistent 
--        {  createORupdateDB :: Int,
--           viewDB :: RuleView

-- }
data UpdatedLogic = UpdatedLogic
    {
        upc :: Integer
        , futureLogic :: FutureLogic
    } deriving (Generic,Show)

data FutureLogic = FutureLogic
    {
        storeExceptions :: [StoreException]
        , zoneExceptions :: [ZoneException]
        , rulec :: Rule
        , effective :: String
    } deriving (Generic,Show)

data StoreException = StoreException
    {
        rule :: Rule
        ,store :: String
        ,zone :: String
    } deriving (Generic,Show)
    
data ZoneException = ZoneException
    {
        rule :: Rule
        ,zone :: String
    } deriving (Generic,Show)

data Rule = Rule
   {
        adjustmentMethod :: String
        ,ignoreClearancePrice :: String
        ,noMarkupIfOnAd:: String
        ,markupBasisPoints :: Integer
    } deriving (Generic,Show)

instance FromJSON Rule
instance FromJSON ZoneException
instance FromJSON StoreException
instance FromJSON FutureLogic
instance FromJSON UpdatedLogic

instance ToRow Rule
