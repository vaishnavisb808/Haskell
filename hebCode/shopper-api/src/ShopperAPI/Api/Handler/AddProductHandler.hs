{-|
    Module          : ShopperAPI.Api.Handler.AddProductHandler
    Description     : Handler for /addproduct
-}

{-# LANGUAGE OverloadedStrings #-}

module ShopperAPI.Api.Handler.AddProductHandler where

import           Control.Monad.Except                       (mplus)
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.Trans.Reader                 (asks)
import           Data.Maybe                                 (Maybe (Nothing))
import           Database.PostgreSQL.Simple.FromField       (name)
import           GHC.Generics
import           Servant                                    (Handler, throwError)
import           ShopperAPI.Api.Types           as AT       (AddProduct (category, currency, 
                                                            productDescription, 
                                                            productName, 
                                                            productPrice, 
                                                            status),
                                                            ApiHandler,
                                                            ApiResponse (ApiResponse))

import           ShopperAPI.Api.MiddleWare.Auth.Types as MT (AccessData (id, role))
import           ShopperAPI.Core.Config.Types               (Env, envDbOps)
import           ShopperAPI.Core.Transformers               (convertAddProductToCore)
import           ShopperAPI.Core.Types                as CT (AddProduct (category, currency, 
                                                            productDescription, productName,
                                                            productPrice, status))
import           ShopperAPI.Core.Utils                      (jsonError400,
                                                            jsonError404,
                                                            jsonError500)
import           ShopperAPI.Services.Database.Types         (AddProductFinalType,
                                                            DbOps (DbOps),
                                                            addProduct,
                                                            checkCategoryName,
                                                            checkProductName)
import           Text.Regex.Posix                           ((=~))


-- | Handler function for addproduct endpoint
addProducts ::  AT.AddProduct -> AccessData -> ApiHandler (ApiResponse String)
addProducts request accessData = do
    handleRequest . convertAddProductToCore =<< validateRequest request
  where
    handleRequest :: CT.AddProduct -> ApiHandler (ApiResponse String)
    handleRequest request'  = do
            dbOps <- asks envDbOps
            checkProductNameResult <- liftIO $ checkProductName dbOps (CT.productName request')
            case checkProductNameResult of
                Left _ -> do
                    checkcategoryResult <- liftIO $ checkCategoryName dbOps (CT.category request')
                    case checkcategoryResult of
                        Left _ -> throwError $ jsonError404 "Category not found"
                        Right categoryid -> do
                            addProductResult <- liftIO $ addProduct dbOps categoryid request'
                            case addProductResult of
                                Right _ -> return $ ApiResponse "The product has been added to the inventory" Nothing 200
                                Left _ -> throwError $ jsonError500 "Adding product failed, try again."
                Right _ -> throwError $ jsonError404 "Product already exists"

-- | Function for validating price
validatePrice :: Float -> Maybe String
validatePrice priceForProduct = if  priceForProduct > 0
                                    then Nothing
                                    else Just "Invalid price"

-- | Function for validating price
validateName :: String  -> Maybe String
validateName nameOfproduct = do
    let checkPattern = (nameOfproduct =~ ("[a-zA-Z0-9@#$%^&-+=()_]$" :: String)) :: Bool
    case nameOfproduct of
        "" -> Just "Please fill all the fields"
        _  ->  if length nameOfproduct < 80 && checkPattern
                    then  Nothing
                    else Just "Invalid name"

-- | Function for validating  category
validateCategory :: String -> Maybe String
validateCategory categoryName = case categoryName of
        "" ->  Just "Please fill all the fields"
        _  -> Nothing

-- | Function for validating product status
validateStatus :: String  -> Maybe String
validateStatus statusCheck' = case statusCheck' of
        "available" ->  Nothing
        "Available" ->  Nothing
        ""          -> Just "Please fill all the fields"
        _           -> Just "Invalid status"

-- | Function for validating currency
validateCurrency :: String  ->  Maybe String
validateCurrency currencycheck' = case currencycheck' of
        "$" -> Nothing
        ""  -> Just "Please fill all the fields"
        _   -> Just "Only $ can be accepted as currency"

--  | Function for validating  the description of product
validateDescription :: String -> Maybe String
validateDescription  description = case description of
        "" -> Just "Please fill all the fields"
        _  -> do
                let checkPattern = (description =~ ("[a-zA-Z0-9@#$%^&-+=()_]$" :: String)) :: Bool
                if  length description < 180 && checkPattern
                    then Nothing
                    else Just "Invalid product description"

-- | validate request body for addproduct endpoint
validateRequest :: AT.AddProduct  -> AT.ApiHandler AT.AddProduct
validateRequest addproduct  = do
    let validationResult  = validateName (AT.productName addproduct) `mplus`
                            validateCategory (AT.category addproduct) `mplus`
                            validatePrice (AT.productPrice  addproduct) `mplus`
                            validateCurrency (AT.currency addproduct) `mplus`
                            validateDescription (AT.productDescription addproduct) `mplus`
                            validateStatus (AT.status addproduct)
    case validationResult of
            Just err -> do
                throwError $ jsonError400 err
            Nothing  -> return  addproduct


