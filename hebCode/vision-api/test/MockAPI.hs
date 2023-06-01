{-# LANGUAGE OverloadedStrings #-}
module MockAPI where

import  VisionApi.ObjectDetection.ObjectDetectionService


data MockAPI  = MockAPI


-- class ObjectDetectionService a where
--     upload :: a -> String -> IO String 
--     objectDetectUrl:: a -> String ->IO(Either String [ImageTag])
--     objectDetectFile::a -> String ->IO(Either String [ImageTag])

instance ObjectDetectionService MockAPI where
    upload api str = error "not implemented"
    objectDetectUrl api str = error "not implemented"
    objectDetectFile api str = error "not implemented" 
