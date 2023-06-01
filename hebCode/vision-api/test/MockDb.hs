{-# LANGUAGE OverloadedStrings #-}
module MockDb where


import VisionApi.Database.DbOps
import VisionApi.Types

data MockDb  = MockDb


getAllImagesResponse = [ImageDetails 1 "ImgLabel1" [ImageTag 100 "mountain"]]


getImageByIdResponse x
    | x == 1 = [ImageDetails 1 "ImgLabel1" [ImageTag 100 "mountain"]]
    | x == 2 = [ImageDetails 2 "ImgLabel2" [ImageTag 200 "tree"]]
getImagesByTagResponse x
    | x == "mountain" = [ImageDetails 1 "ImgLabel1" [ImageTag 100 "mountain"]]
    | x == "tree" = ([ImageDetails 2 "ImgLabel2" [ImageTag 200 "tree"],ImageDetails 1 "ImgLabel1" [ImageTag 100 "mountain"]])



instance DbOps MockDb where
    getAllImages db = return (Right getAllImagesResponse)
    getImageById db imageId = do
        if imageId == 5
            then return (Left "Server Error : Unable to fetch image with imageId :5, Try again")
            else 
                 return (Right ([getImageByIdResponse imageId]))
    getImagesByTag db imageTags = do
         let tag = head imageTags
         if tag == "pen"
             then return (Left "Server Error : Unable to fetch images with given object. Try again")
             else 
                  return (Right (getImagesByTagResponse (head imageTags))) 

