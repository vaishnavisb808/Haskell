{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module VisionApi.ObjectDetection.ImaggaService where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString as B
import           Data.ByteString.Lazy as BS
import           Data.Int (Int)
import           Data.Text.Lazy.Encoding
import           Database.PostgreSQL.Simple
import           GHC.Base (Float)
import           GHC.Generics
import           GHC.Int
import           Network.HTTP.Req
import           Servant (Handler)
import           VisionApi.ObjectDetection.ObjectDetectionService
import           VisionApi.ObjectDetection.Types
import           VisionApi.Types


import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Char8 as Bytechar
import qualified Network.HTTP.Client.MultipartFormData as LM


-- | datatypes holds auth information and is an instance of ObjectDetectionService
data BasicAuth= BasicAuth {
    token::BSC8.ByteString
}deriving(Generic,Show)



instance ObjectDetectionService BasicAuth where
    -- | make request to /uploads endpoint of Imagga, with image as url
    upload  auth img = do
      body <-
        reqBodyMultipart
            [ LM.partLBS "image_base64" (encodeUtf8 img)
            ]
      imaggapost <- runReq defaultHttpConfig $ do
        r <-
            req
            POST
            (https "api.imagga.com"/:"v2"/:"uploads")
            body
            jsonResponse
            (header "Authorization" (token auth))
        return (responseBody r::Image)
      if statusType (uploadStatus imaggapost) == "success"
         then do
              let upld = upload_id $ result1 imaggapost
              return $Right upld
         else return $Left $text$uploadStatus imaggapost


    {- | function to make request to /tags endpoint of Imagga using url
         Not implemented yet
     -}
    objectDetectUrl auth url   = do
      let setLimit = 20 :: Int
      let setThreshold = 30 :: Float
      result2 <- runReq defaultHttpConfig $ do

        let qparam =("image_url" =: url) <>
                ("limit" =: setLimit ) <>
                ("threshold" =: setThreshold) <>
                (header "Authorization" (token auth))
        r <-
            req
            GET
            (https "api.imagga.com"/:"v2"/:"tags")
            NoReqBody
            jsonResponse
            qparam
        return (responseBody r::ImageD)
      if statusType(status result2) ==  "success"
         then do
             let taglist = tags $VisionApi.ObjectDetection.Types.result result2
             let out = convertType taglist
             return $ Right out
         else return $Left $"Object Detection failed: "++text(status result2)


    -- | function to make request to /tags endpoint of Imagga using imagebase64
    objectDetectFile auth uploadId = do
      let setLimit = 20 :: Int
      let setThreshold = 20 :: Float
      result2 <- runReq defaultHttpConfig $ do
            let qparam = ("image_upload_id" =: uploadId) <>
                         ("limit" =: setLimit ) <>
                         ("threshold" =: setThreshold) <>
                         (header "Authorization" (token auth))
            r <-
              req
              GET
              (https "api.imagga.com"/:"v2"/:"tags")
              NoReqBody
              jsonResponse
              qparam
            return (responseBody r::ImageD)
      if statusType(status result2) ==  "success"
         then do
              let taglist = tags $VisionApi.ObjectDetection.Types.result result2
              let out = convertType taglist
              return $ Right out
         else return $Left $"Error response from imagga"++text(status result2)


-- | convert from response type to core types
convertType :: [ImgTag] -> [ImageTag]
convertType [] = []
convertType taglist  = do
      let conf = VisionApi.ObjectDetection.Types.confidence $ Prelude.head taglist
      let en1 = en $ VisionApi.ObjectDetection.Types.tag $ Prelude.head taglist

      let img = [ImageTag conf en1]
      img++convertType (Prelude.tail taglist)

