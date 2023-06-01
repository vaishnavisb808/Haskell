{-|
    Module : ShopperAPI.Api.Handler.ViewProductsByRatingHandler
    Description : Handler for /getproducts
    Handler for /getproducts

-}


module ShopperAPI.Api.Handler.ViewProductsByRatingHandler where

import           Servant                             (Handler, throwError)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Reader          (asks)
import           ShopperAPI.Api.Types                
import           ShopperAPI.Core.Config.Types        (Env, envDbOps)
import           ShopperAPI.Core.Transformers        (convertProductListToResponse)
import           ShopperAPI.Core.Utils               (jsonError400,
                                                      jsonError404,
                                                      jsonError500)
import           ShopperAPI.Services.Database.Types   as D   (DbOps (DbOps), viewProducts)
import           Data.Maybe                          (isJust, fromMaybe)



-- | Handler function for viewproducts endpoint.
getProductsHandler :: Maybe String
                   -> Maybe Integer
                   -> ApiHandler (ApiResponse Response)
getProductsHandler sortby offset = do
        dbOps <- asks envDbOps
        sortby' <- validateSortBy sortby
        offset' <- validateOffset offset
        dbresult <- liftIO $ D.viewProducts dbOps sortby' offset'
        case dbresult of
            Left e -> throwError $ jsonError500 ( "sql error "++ show e )
            Right ([],nextPageKey) ->
                if isJust offset
                then  throwError $
                            jsonError404 $ "No data found for offset "
                                            ++ show offset
                else throwError $
                            jsonError404  "No data to display "
            Right (result,nextPageKey) ->
                return $ ApiResponse ( convertProductListToResponse
                                                result
                                                nextPageKey
                                         )
                                         Nothing
                                         200

-- | validation function for validate sortBy query parameter
validateSortBy :: Maybe String -> ApiHandler (Maybe String)
validateSortBy sortBy = if sortBy /= Just "rating" && isJust sortBy
                           then throwError $ jsonError400 "invalid input for query parameter sortBy"
                           else return sortBy


-- | validation function for validate offset
validateOffset :: Maybe Integer -> ApiHandler (Maybe Integer)
validateOffset offset =
    case offset of
        Just offset' -> if offset' >= 0
                        then return $ Just offset'
                        else throwError $
                            jsonError400 "Offset should be an positive integer or zero"
        Nothing -> return Nothing
