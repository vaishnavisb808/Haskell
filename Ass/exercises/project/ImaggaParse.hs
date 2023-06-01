{-# LANGUAGE OverloadedStrings #-}
module ImaggaParse where
import Data.Text as T
import Data.Aeson

data ImageDetails = ImageDetails{
    confidence :: Double,
    label :: [InputData]
    } deriving(Generic,Show)

data ImageDetails' = ImageDetails'{
    tags :: [ImageDetails]
    } deriving(Generic,Show)

data ImageDetails'' = ImageDetails'' {
    result :: [ImageDetails']
   } deriving(Generic,Show)

data InputData= InputData {
    url::String}
    deriving(Generic,Show)

instance FromJSON ImageDetails''
instance FromJSON ImageDetails'
instance FromJSON ImageDetails
instance FromJSON InputData

mains = do
    inp <- readFile "imagres.json"
    print inp