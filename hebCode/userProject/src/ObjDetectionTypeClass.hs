{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ObjDetectionTypeClass where

import GHC.Generics
import CoreTypes

-- data ImageTag = ImageTag{
--     confidance :: Double,
--     tag1 :: String
-- } deriving(Generic,Show)

data Authenticate= Authenticate {
    auth::String 
}deriving(Generic,Show)

class ObjectDetection a where
    upload :: a -> String -> String  
    objectDetectUrl:: a -> String ->[ImageTag]
    objectDetectFile::a -> String ->[ImageTag]
