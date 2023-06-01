{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Data.Map
--import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8

data Person = Person {
      name :: T.Text
    , age  :: Int
    } deriving Show
instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "name"
        <*> v .: "age"
        
instance ToJSON Person where
    -- this generates a Value
    toJSON (Person name age) =
        object ["name" .= name, "age" .= age]

    -- this encodes directly to a bytestring Builder
    toEncoding (Person name age) =
        pairs ("name" .= name <> "age" .= age)

main = do 
    --print (encode (Person {name = "Joe", age = 12}))
    --print(decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person)
    --print (encode (Person {name = "Joe", age = 12}))
    print (decode "[1,2,3]" :: Maybe [Int])
    print (decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int))
    print (decode "{\"foo\": [\"abc\",\"def\"]}" :: Maybe Value)


        

--instance ToJSON Person where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    --toEncoding = genericToEncoding defaultOptions

--instance FromJSON Person
    -- No need to provide a parseJSON implementation.