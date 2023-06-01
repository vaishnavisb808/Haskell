{-# LANGUAGE OverloadedStrings #-}
module JsonParse where
import Data.Text as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
import Data.Map as Map
--import Data.Aeson.Types
import CryptoPlus
import Data.Maybe

data Contains = Contains {
    name ::T.Text
    , message :: T.Text
} deriving Show 
--data User = User [Contains]  deriving Show
instance FromJSON Contains where
    parseJSON = withObject "Contains" $ \obj -> Contains
        <$> obj .: "name"
        <*> obj .: "message"

instance ToJSON Contains where
     -- this generates a Value
    toJSON (Contains name message) =
        object ["name" .= name, "message" .= message]
    -- this encodes directly to a bytestring Builder
    toEncoding (Contains name message) =
        pairs ("name" .= name <> "message" .= message) 

main = do
    inp <- readFile "firstTask.json"
    decodeMessage (B.packChars inp)
decodeMessage  m =  do
    let op = decode m :: Maybe(Map String [Contains])
    --print op
    if isNothing op
        then putStrLn "empty file"
    else do
        let actualOp = (\ (Just x) -> x) op
    --print actualOp
    --let listOfpairs =  [Contains]
   -- print listOfpairs
        putStrLn "Enter name: "
        msg <- getLine
        let users'  = Map.lookup "users" actualOp
        let check = getMessage (T.pack msg) ((\(Just x) -> x) users')
        print $ decryptMessage 2 $ T.unpack check
--extract = do        

getMessage :: T.Text->[Contains]->T.Text
getMessage character [] = ""
getMessage character (x:xs) | name x == character =  message x
                            | otherwise = getMessage character xs    

