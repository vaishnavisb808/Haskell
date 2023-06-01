{-|
    Module      : ShopperAPI.Api.Handler.ViewWishlistHandler
    Description : Handler functions for /viewwishlist endpoint
-}
module       ShopperAPI.Api.Handler.ViewWishlistHandler where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Reader
import           Servant                              (throwError)
import           ShopperAPI.Api.MiddleWare.Auth.Types as AT (AccessData, id)
import           ShopperAPI.Api.Types                 (ApiHandler,
                                                       ApiResponse (ApiResponse),
                                                       WishList)
import           ShopperAPI.Core.Config.Types         (Env (envDbOps))
import           ShopperAPI.Core.Transformers         (convertWishlistCoreToApi)
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types   (getWishlistData)
import           ShopperAPI.Services.Logger.Logger    (viewWishlistHandleLog,
                                                       withTimeLog)
import           ShopperAPI.Services.Logger.Types

viewWishlistUser :: AccessData
                 -> ApiHandler (ApiResponse [WishList])
viewWishlistUser accessData = withTimeLog ViewWishlistHandle $ do
     let logger = viewWishlistHandleLog
         userId = AT.id accessData
     dbOps <- asks envDbOps
     wishListData <- liftIO $ getWishlistData dbOps userId
     case wishListData of
         Left err          -> do
             logger ERROR ("Failed to fetch wishlist details " ++ show err) 500
             throwError $ jsonError500 "failed to fetch data for wishlist"
         Right []          -> do
            logger INFO "wishlist is empty" 404
            throwError $ jsonError404 "wishlist is empty"
         Right wishListData -> do
             logger INFO "All items in wishlist are listed" 200
             return $ ApiResponse (fmap convertWishlistCoreToApi wishListData) Nothing 200
