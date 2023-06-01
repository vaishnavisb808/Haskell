{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module LabIntoDb where
import GHC.Generics
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
-- | data type for each object in the image
data ImageTag = ImageTag{
    confidence :: Double,
     tag :: String
} deriving(Generic,Show)
-- | data type of an image
data ImageDetails = ImageDetails{
    imageId::Int,
    label :: String,
    imagetag :: [ImageTag]
} deriving(Generic,Show)

instance ToRow ImageDetails where
    toRow imgDetails = [toField(imageId imgDetails), toField(label imgDetails)] 

labelInsert=do
    let img_label="fine.png"::String
    conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
    
    rows <-execute conn "insert into image_details (image_label) values (?)" (Only img_label)
    print rows