{-# LANGUAGE OverloadedStrings #-}
module DbOps where


import           CoreTypes
--import           Servant
import           Data.Pool
import           Database.PostgreSQL.Simple
import qualified Data.Text as Text
import qualified Data.Map as Map
import Database.PostgreSQL.Simple (Only)


type Tag = Text.Text
type ImageId = Int


class DbOps d where 
    getAllImages::d -> IO [ImageDetails]
    getImageById::d -> ImageId -> IO ImageDetails
    getImagesByTag::d -> [Tag] -> IO [ImageDetails]
    insertImageToDb :: d -> String -> String -> IO Int


data PostgresDb = PostgresDb {
    pgDbConnectionPool :: Pool Connection
}


instance  DbOps PostgresDb where
    getAllImages db = do
        let conns = pgDbConnectionPool db 
        objects <- withResource conns $ \conn->
                     ((query_ conn "SELECT image_details.image_id,image_label,\
                                          \confidence,tag \
                                   \FROM image_details \
                                   \INNER JOIN image_tags \
                                   \ON image_details.image_id=image_tags.image_id ") 
                                   ::IO[(Int,String,Double,String)])
        let reqMap = createImageIdTagMap objects Map.empty

        let allimages = createFinalList objects reqMap
        return allimages
        
    getImageById db imageId= do 
        let conns = pgDbConnectionPool db
        images <-  withResource conns $ \conn ->
            query conn " SELECT image_details.image_id, image_label, confidence, tag \
                        \FROM image_details  \
                        \inner join image_tags  \
                        \ON image_details.image_id=image_tags.image_id \
                        \WHERE image_details.image_id=?"[imageId]
        let tags = fmap(\(_,_,confidence,tag)->ImageTag confidence tag) images
        let details = (\(image_id,label,_,_)->ImageDetails image_id label tags)$ Prelude.head images
        return details

    getImagesByTag db splitData = do
        let conns = pgDbConnectionPool db
        listOfImages<- withResource conns $ \conn ->
            query conn "SELECT image_details.image_id,image_label \
                       \FROM image_details \
                       \INNER JOIN \
                                \( SELECT image_id FROM image_tags \ 
                                \WHERE tag IN ? \
                                \GROUP BY image_id \
                                \HAVING COUNT(tag)= ?) \
                      \AS i \
                      \ON image_details.image_id=i.image_id; "  (In splitData,Prelude.length splitData)
        let images=  fmap(\(image_id,label) -> ImageDetails image_id label [])listOfImages
        return images

    insertImageToDb db uploadId label= do
        let conns = pgDbConnectionPool db
        xs <-  withResource conns $ \conn ->
          query conn "INSERT INTO public.image_details (upload_id,image_label) \
                     \VALUES (?,?) RETURNING imageid " (uploadId::String,label::String) :: IO [Only Int]
        let imageid = fromOnly $ Prelude.head xs
        return imageid 


-- | Creating a map consist of imageId and imageTags
createImageIdTagMap [] map = map 
createImageIdTagMap ((image_id,image_label,confidence,tag):xs) currentMap = do
    let map = Map.insertWith (++) image_id [ImageTag confidence tag] currentMap   
    createImageIdTagMap xs map

{- 
Removed duplicate values of imageId
Got the final list with imageId label and corresponding imageTags 
with confidence and tag
-}
createFinalList::[(Int,String,Double,String)]->Map.Map Int [ImageTag]->[ImageDetails] 
createFinalList [] _ =  []
createFinalList currentList duplicateMap = do
    let id = (\(x,_,_,_)->x) (Prelude.head currentList) 
    let label =  (\(_,x,_,_)->x) (Prelude.head currentList)
    let tag  = Map.lookup id duplicateMap
    let pureMap = Map.delete id duplicateMap
    case tag of
        Just tags -> do
         [ImageDetails id label tags] ++  createFinalList (Prelude.tail currentList) pureMap
        Nothing -> do createFinalList (Prelude.tail currentList) duplicateMap

main = do
    let conn =  initializeConnection 
    connPool <- initConnectionPool conn
    let postgresDb = PostgresDb connPool

    imid<- insertImageToDb postgresDb "uploadId" "label"
    return imid


initConnectionPool connectionInitialize = do
  createPool connectionInitialize
             close
             1 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe


initializeConnection  = do
    connect defaultConnectInfo{
      connectDatabase= "postgres",
      connectUser = "postgres",
      connectPassword = "Minnus"
    }      