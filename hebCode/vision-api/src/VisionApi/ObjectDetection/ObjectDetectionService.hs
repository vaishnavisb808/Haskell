{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module VisionApi.ObjectDetection.ObjectDetectionService where

import           GHC.Generics
import           VisionApi.Types
import qualified Data.Text.Lazy as T

-- Typeclass for Object Detection
class ObjectDetectionService a where
    upload :: a -> T.Text -> IO (Either String String)
    objectDetectUrl:: a -> T.Text ->IO(Either String [ImageTag])
    objectDetectFile::a -> T.Text ->IO(Either String [ImageTag])
