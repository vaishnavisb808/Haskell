{-|
    Module          : ShopperAPI.Api.Handler.EditProductHandler
    Description     : Handler for /editproduct
-}

module ShopperAPI.Api.Handler.EditProductHandler where
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Servant
import           ShopperAPI.Api.Types               as AT (ApiHandler,
                                                           ApiResponse (ApiResponse),
                                                           EditProduct (..))
import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Core.Transformers        (convertToFinalTypeForEditProduct)
import           ShopperAPI.Core.Types              as CT
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types as DT
import           ShopperAPI.Services.Logger.Logger
import           ShopperAPI.Services.Logger.Types   as LT

-- | Handler function for EditProduct endpoint.
editProductDetails :: AT.EditProduct -> ApiHandler (ApiResponse String)
editProductDetails request = do
        let productId = AT.productId request
        if productId > 0
          then do
          dbOps <- asks envDbOps
          status <- liftIO $ productStatus dbOps productId
          case status of
            Right _ -> do
              let categoryName = AT.category request
              categoryId <- liftIO $ checkCategory  dbOps categoryName
              case categoryId of
                  Left _ -> throwError $ jsonError400 "category name not exist"
                  Right categoryid   -> do
                    let productDetails = convertToFinalTypeForEditProduct request categoryid
                    dbresult <- liftIO $ editInfo dbOps productId productDetails
                    case dbresult of
                     Right editInfo -> return $ ApiResponse "product has been successfully updated" Nothing 200
                     Left msg -> throwError $ jsonError400 "product updation failed"
            Left msg -> throwError $ jsonError400 "product not available"
        else throwError $ jsonError400 "invalid productId"
