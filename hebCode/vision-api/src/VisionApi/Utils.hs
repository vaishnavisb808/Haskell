{-# LANGUAGE OverloadedStrings #-}
module VisionApi.Utils where

import           Text.Regex.Posix((=~))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TEnc
import qualified Data.Text.Encoding.Base64 as B64


-- | validate whether a given string is a Url
isUrl :: T.Text -> Bool
isUrl (url) = T.unpack(url) =~ regexPattern
    where regexPattern::String
          regexPattern = "(https://|http://)[a-zA-Z./_0-9-]*"


-- | Validate whether a given base64String is Valid
isValidBase64:: T.Text -> Bool
isValidBase64 str = B64.isValidBase64 (T.toStrict str)


-- | remove the header from base64String
stripHeader base64String = do
    if T.null base64String
       then Nothing
       else do
           let base64String' = T.split (==',') base64String
           if null base64String' || length  base64String' < 2
              then Nothing
              else Just $base64String'!!1



-- | gives approximation of image file size:
sizeOfBase64::T.Text -> Double
sizeOfBase64 base64String = do
   let lengthOfString = T.length base64String
   let approxNoOfBytes = fromIntegral lengthOfString * (fromIntegral 3 / fromIntegral 4)
   ( approxNoOfBytes / 1000000)


