{-# LANGUAGE OverloadedStrings #-}

{-|
    description: DbOps typeclass provides standard interface all database operations
                 within the api.
-}
module VisionApi.Database.DbOps where


import Control.Monad.Catch
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import VisionApi.Types

import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text




{-| Typeclass provides a standard interface for all db operations withing API.
    This allows for abstraction of database dependencies from the rest of the Server.
-}
class DbOps d where
    -- | initilise database with necessary tables if not already present
    initializeSchema :: d -> IO (Either String Int)
    -- | get all images from database
    getAllImages::d -> IO (Either String [ImageDetails])

    -- | get a single image using imageId
    getImageById::d -> Int -> IO (Either String [ImageDetails])

    -- | get images that contain the list of specified tags
    getImagesByTag::d -> [Text.Text] -> IO (Either String [ImageDetails])

    -- | insert metadata for one image to the DB
    insertImageToDb :: d -> String -> String  -> IO (Either String Int)

    -- | insert image metadata and associated tags into DB
    insertImgAndTagsToDb::d ->  String -> String -> [ImageTag] -> IO (Either String Int)


