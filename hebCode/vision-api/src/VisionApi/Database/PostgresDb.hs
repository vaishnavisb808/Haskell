{-# LANGUAGE OverloadedStrings #-}

module VisionApi.Database.PostgresDb where

import Control.Monad.Catch
import qualified Data.Map as Map
import Data.Pool
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import VisionApi.Database.DbOps
import VisionApi.Types

data PostgresDb = PostgresDb {
    pgDbConnectionPool ::Pool Connection
}


instance  DbOps PostgresDb where
    -- | Script to initialize database
    initializeSchema db = catch(do
        let conns = pgDbConnectionPool db
        withResource conns $ \conn-> do
            execute_ conn "CREATE TABLE IF NOT EXISTS image_details \
                         \(image_id SERIAL PRIMARY KEY not null , upload_id varchar(512), \
                         \upload_date timestamptz not null default CURRENT_TIMESTAMP,\
                         \object_detection bool,image_label varchar(512) not null);"

            execute_ conn "CREATE TABLE IF NOT EXISTS image_tags \
                         \(image_id int NOT NULL,confidence Float NOT NULL,\
                            \tag varchar(512),\
                         \FOREIGN KEY (image_id) REFERENCES image_details(image_id));"
            return $Right 0

            ) handler

      where
        handler::SomeException->IO (Either String Int)
        handler ex = return (Left $" SQL Error : " ++ show ex)



    {- get all images and associated tags from DB
       returns Left errorMessage if any exception happens during execution
    -}
    getAllImages db = do
        catch ( do
             let conns = pgDbConnectionPool db
             withResource conns $ \conn-> do
                 rows <-(query conn "SELECT     image_details.image_id,image_label,\
                                                \confidence,tag \
                                    \ FROM       image_details \
                                    \ INNER JOIN image_tags \
                                    \ ON         image_details.image_id=image_tags.image_id \
                                    \ ORDER BY   image_id \
                                    \ LIMIT      ?"[limit]
                                    ::IO [(Int,String,Double,String)])
                 let reqMap = createImageIdTagMap rows Map.empty
                 let allimages = createImageList rows reqMap
                 return $Right allimages
           ) handler
      where
        limit :: Int
        limit = 30

        handler::SomeException->IO (Either String [ImageDetails])
        handler ex = return (Left $" SQL Error : " ++ show ex)


    {- get a specific image using its imageId
       returns Left errorMessage if any exception happens during execution
    -}
    getImageById db imageId= do
         catch (
                do
                let conns = pgDbConnectionPool db
                withResource conns $ \conn ->do
                    rows <-query conn
                        "SELECT     image_details.image_id, image_label, confidence, tag \
                        \FROM       image_details  \
                        \INNER JOIN image_tags  \
                        \ON         image_details.image_id=image_tags.image_id \
                        \WHERE      image_details.image_id=?" [imageId]
                    if null rows
                        then return $Right []
                        else do
                            let tags = fmap (\(_,_,confidence,tag)->ImageTag confidence tag) rows
                            let details = (\(image_id,label,_,_)->ImageDetails image_id label tags)
                                    $ Prelude.head rows
                            return $Right [details]) handler
      where
        handler::SomeException->IO (Either String [ImageDetails])
        handler ex = return (Left $" SQL Error : " ++ show ex)


    {- get images that contain the specified set of objects within them
       returns Left errorMessage if any exception happens during execution
    -}
    getImagesByTag db splitData = do
        catch ( do
            let conns = pgDbConnectionPool db
            withResource conns $ \conn -> do
                lstofimgs <- query conn "SELECT     image_details.image_id,image_label,confidence,tag \
                                        \FROM       image_details \
                                        \INNER JOIN ( \
                                                      \SELECT   image_id \
                                                      \FROM     image_tags \
                                                      \WHERE    tag \
                                                      \IN ? \
                                                      \GROUP BY image_id \
                                                      \HAVING   COUNT(tag)= ?) \
                                        \AS i \
                                        \ON         image_details.image_id=i.image_id \
                                        \INNER JOIN image_tags \
                                        \ON         i.image_id = image_tags.image_id;"
                                        (In splitData,Prelude.length splitData)
                
                let reqMap = createImageIdTagMap lstofimgs Map.empty
                let allimages = createImageList lstofimgs reqMap
                return $Right allimages) handler
      where
        handler::SomeException->IO (Either String [ImageDetails])
        handler er = return (Left $" SQL Error : "  ++ show er)


    -- insert new image to DB
    insertImageToDb db uploadId label = do
        catch (
            do
            let conns = pgDbConnectionPool db
            withResource conns $ \conn -> do
                xs <- query conn "INSERT INTO public.image_details (upload_id,image_label,object_detection) \
                                 \VALUES (?,?,?) RETURNING image_id "
                                 (uploadId::String,label::String,False) :: IO [Only Int]
                let imageid = fromOnly $ Prelude.head xs
                return $ Right imageid ) handler
      where
        handler::SomeException->IO (Either String Int)
        handler er = return (Left $" SQL Error : "  ++ show er)


    -- function to add new image and tags in DB
    insertImgAndTagsToDb db uploadId label objects = do
        catch (do
            let conns = pgDbConnectionPool db
            withResource conns $ \conn -> do
                withTransaction conn $ do
                    xs <- query conn "INSERT INTO public.image_details (upload_id,image_label,object_detection) \
                                     \VALUES (?,?,?) RETURNING image_id "
                                     (uploadId::String,label::String,True) :: IO [Only Int]
                    let imageid =fromOnly $ Prelude.head xs
                    let rows = map (\tag' -> (imageid,confidence tag', tag tag'))objects
                    xs <- executeMany conn "INSERT INTO public.image_tags(image_id,confidence,tag)\
                    \VALUES (?,?,?)" rows
                    if (xs >0 ) then return $ Right imageid
                    else return $ Left "Image tag list is empty"
            ) handler
      where
        handler::SomeException->IO (Either String Int)
        handler er = return (Left $"SQL Error" ++ show er)



-- | Creating a map consist of imageId and imageTags
createImageIdTagMap [] map = map
createImageIdTagMap ((image_id,image_label,confidence,tag):xs) currentMap = do
    let map = Map.insertWith (++) image_id [ImageTag confidence tag] currentMap
    createImageIdTagMap xs map

{- Removed duplicate values of imageId
   Got the final list with imageId label and corresponding imageTags
   with confidence and tag
-}
createImageList::[(Int,String,Double,String)]->Map.Map Int [ImageTag]->[ImageDetails]
createImageList [] _ =  []
createImageList currentList duplicateMap = do
    let id = (\(x,_,_,_)->x) (Prelude.head currentList)
    let label =  (\(_,x,_,_)->x) (Prelude.head currentList)
    let tag  = Map.lookup id duplicateMap
    let pureMap = Map.delete id duplicateMap
    case tag of
        Just tags -> do
            ImageDetails id label tags: createImageList (Prelude.tail currentList) pureMap
        Nothing -> do createImageList (Prelude.tail currentList) duplicateMap
