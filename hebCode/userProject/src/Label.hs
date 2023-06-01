{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Label where
import GHC.Generics
import Data.List
import ObjDetectionTypeClass
import CoreTypes
-- data ImageDe = ImageDe{
--     tags :: [ImgTag]
--    } deriving(Generic,Show)

-- data ImgTag = ImgTag{
--     confidence :: Double,  -- ^ Confidence score of detected object
--     tag :: ImageLabel     -- ^ Name of the detected object in the given image
-- } deriving(Generic,Show,Eq)


-- data ImageLabel = ImageLabel {
--     en::String
--     } deriving(Generic,Show,Eq)

-- data Upload = Upload {
--     upload_id :: String
-- } deriving (Generic,Show) 
-- | data type for each object detected in the image 
-- data ImageTag = ImageTag{
--     confidence :: Double,  -- ^ Confidence score of detected object
--     tag :: String      -- ^ Name of the detected object in the given image
-- } deriving(Generic, Show)
-- -- | data type of an image
-- data ImageDetails = ImageDetails{
--     upload_id :: String, -- ^ Upload id of an image
--     imagetags :: [ImageTag] 
-- } deriving(Show,Generic)
-- | Function to generate unique label
generateUnique :: String->[ImageTag] -> IO [Char]
generateUnique uniqueId details= do
    --let uniqueId = upload_id upId   -- ^ taking upload id from ImageDetails
    --let listTags =  tags details  -- ^ storing list of ImageTag in a variable
    if not(Prelude.null details) then do
        let high_confident_tags = Prelude.take 2 $ details
        let len = Prelude.length high_confident_tags
        let head_high_confident_tags = Prelude.head high_confident_tags
        let first_tag =  tag1 head_high_confident_tags
        if len == 1 then
            return (uniqueId ++ "_" ++ first_tag)
        else do
            let tail_high_confident_tags = Prelude.tail high_confident_tags
            let second_tag_list = Prelude.head tail_high_confident_tags
            let second_tag = tag1 second_tag_list
            return (uniqueId ++ "_" ++ first_tag ++ "&" ++ second_tag)
    else
        return uniqueId