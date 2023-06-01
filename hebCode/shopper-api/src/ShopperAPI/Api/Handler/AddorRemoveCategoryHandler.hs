{-|
    Module : ShopperAPI.Api.Handler.ManagerHandlers
    Description : Handler functions for adding and removing Product Category 
-}


module ShopperAPI.Api.Handler.AddorRemoveCategoryHandler(addCategory,deleteCategory) where 

import              GHC.Generics
import              ShopperAPI.Api.Types                  as AT  
import              Servant
import              ShopperAPI.Services.Database.Types as DB
import              ShopperAPI.Core.Types 
import              ShopperAPI.Services.Database.Postgres
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Reader 
import              ShopperAPI.Core.Config.Types
import              ShopperAPI.Core.Utils
import              Text.Regex.Posix ((=~)) 
import              ShopperAPI.Api.MiddleWare.Auth.Types as ST 


addCategory :: Maybe String ->ApiHandler( ApiResponse String) 
addCategory  category = do 
    case category of 
        Nothing -> throwError $ jsonError500 "category not given"
        (Just category) -> handleAddCategory  =<< validateCategory category
    
-- | Handler function for adding Product Category
handleAddCategory :: String ->ApiHandler( ApiResponse String)
handleAddCategory categoryName = do 
        dbOps <- asks envDbOps
        categoryId <- liftIO $ checkCategory' dbOps categoryName
        case categoryId of 
            Right [] -> do 
                addedCategoryId <- liftIO $  addCategory' dbOps categoryName
                case addedCategoryId of
                    Right addedCategoryId -> return $ ApiResponse "Product Category added" Nothing 200
                    Left msg -> throwError $ jsonError500 "SQL Error"
            Right categoryId -> throwError $ jsonError400 "Product Category Already exists" 
            Left msg -> throwError $ jsonError500 "SQL Error"         

-- | Handler function for removing Product Category 
deleteCategory :: Integer ->ApiHandler( ApiResponse String) 
deleteCategory categoryId = do 
         dbOps <- asks envDbOps
         productIdsInCategory <- liftIO $ productsWithCategory dbOps categoryId
         case productIdsInCategory of
            Right [] -> do
                deletedCategory <- liftIO $ removeCategory dbOps categoryId
                case deletedCategory of
                    Right 0 -> throwError $ jsonError400 "Category id does not Exist" 
                    Right 1 -> return $ ApiResponse "Product Category removed" Nothing 200
                    Left msg -> throwError $ jsonError500 "SQL Error"
            Right productIdsInCategory -> throwError $ jsonError400 "There are products existing in the product category" 
            Left msg -> throwError $ jsonError500 "SQL Error"

-- Function for validating the Category name 
-- (Category should be Alphabets with maximum character limit 80) 
validateCategory ::  String  -> ApiHandler String  
validateCategory category = do
    let checkCategory' = (category =~ ("^[a-zA-z]*[a-zA-z]$" :: String) ) :: Bool   
    if (length category <= 80) && checkCategory'
        then return category
        else throwError $ jsonError500 "Category should contain only Alphabets with max Character limit 80"      
        