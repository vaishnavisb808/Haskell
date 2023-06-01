{-
    Transformers Module
    exports convertProductListToResponse function to convert list of ProductDetailsWithId
    type in Core types to Response type in Api type
-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module ShopperAPI.Core.Transformers (convertProductListToResponse,
                                     convertToProductResponse,
                                     convertAddressApiToCore,
                                     convertProductStockApiToCore,
                                     convertWishlistCoreToApi,
                                     convertViewCartCoreToApi ,
                                     convertCoreToApi,
                                     convertProductDetailsCoreToApi,
                                     convertToFinalTypeForAddProduct,
                                     convertTrackingApiToCore,
                                     convertToFinalTypeForEditProduct,
                                     convertToFinalTypeForAddProduct,
                                     convertAddProductToCore, convertViewCartCoreToApi,
                                     convertTempAPiToCore,
                                     convertAddProductToCore,
                                     convertApiOrderStatusToCore,
                                     convertAddProductToCore,
                                     convertAddProductToCore,
                                     convertApiOrderStatusToCore,
                                     convertAddCommentsApiToCore
                                      ) where 
                                    
import ShopperAPI.Core.Types as Core ( AddProduct (AddProduct),
                                        EditProduct (EditProduct),
                                        ProductDetailsWithId(ProductDetailsWithId),
                                        ProductDetails(ProductDetails),
                                        ProductStock(ProductStock),
                                        Address(Address) ,
                                        ProductCart(ProductCart) ,
                                        AddComments(AddComments),
                                        Address(Address),Tracking(Tracking),
                                         AcceptOrReject (Accept, Reject),
                                        AddTempValueInfo'(AddTempValueInfo'),
                                         WishList (WishList))
import ShopperAPI.Api.Types   as Api ( ProductDetailsWithId(ProductDetailsWithId),
                                        AddProduct (AddProduct),EditProduct (EditProduct),
                                        ProductDetails(ProductDetails),
                                        ProductResponse(ProductResponse),
                                        Response(Response),
                                        Address(Address),
                                        ProductStock(ProductStock) ,
                                        AddComments(AddComments),
                                        ProductCart(ProductCart),
                                        Tracking(Tracking),
                                        AcceptOrReject(Accept, Reject),
                                        AddTempValueInfo(AddTempValueInfo),
                                         WishList (WishList))
import ShopperAPI.Services.Database.Types as DT  (AddProductFinalType (AddProductFinalType))


-- | convert Core ProductDetails type to Api ProductDetails type
convertProductDetailsCoreToApi :: Core.ProductDetails ->  Api.ProductDetails
convertProductDetailsCoreToApi (Core.ProductDetails productName
                                      productDescription
                                      productCategory
                                      productPrice
                                      currency
                                      temporaryPrice
                                      numberOfReviews
                                      rating
                                      images) =
                                          Api.ProductDetails
                                                 productName
                                                 productDescription
                                                 productCategory
                                                 productPrice
                                                 currency
                                                 temporaryPrice
                                                 numberOfReviews
                                                 rating
                                                 images

-- | convert list of Core ProductDetails type to Api ProductResponse type
convertToProductResponse productList  = Api.ProductResponse (map convertProductDetailsCoreToApi productList)


-- | convert Core ProductDetailsWithId type to Api ProductDetailsWithId type
convertCoreToApi :: Core.ProductDetailsWithId ->  Api.ProductDetailsWithId
convertCoreToApi (Core.ProductDetailsWithId productName
                                            productId
                                            description
                                            productCategory
                                            productPrice
                                            currency
                                            temporaryPrice
                                            numberOfReviews
                                            rating
                                            status
                                            images) =
                                                Api.ProductDetailsWithId
                                                        productName
                                                        productId
                                                        description
                                                        productCategory
                                                        productPrice
                                                        currency
                                                        temporaryPrice
                                                        numberOfReviews
                                                        rating
                                                        status
                                                        images

-- | convert list of Core ProductDetailsWithId type to Api Resonse type
convertProductListToResponse :: [Core.ProductDetailsWithId] -> Maybe Integer ->  Api.Response
convertProductListToResponse prodList = Api.Response (fmap convertCoreToApi prodList)


convertAddressApiToCore :: Api.Address -> Core.Address
convertAddressApiToCore (Api.Address recipientFirstName
                                     recipientLastName
                                     addressLine1
                                     addressLine2
                                     landMark
                                     cityAndState
                                     zipcode
                                     country
                                     communicationNumber ) =
                                        Core.Address recipientFirstName
                                                     recipientLastName
                                                     addressLine1
                                                     addressLine2
                                                     landMark
                                                     cityAndState
                                                     zipcode
                                                     country
                                                     communicationNumber

convertProductStockApiToCore :: Api.ProductStock -> Core.ProductStock
convertProductStockApiToCore (Api.ProductStock productId stockCount) =
    Core.ProductStock productId stockCount

-- | Transformer function to convert Api type to Core type
convertAddCommentsApiToCore :: Api.AddComments -> Core.AddComments
convertAddCommentsApiToCore (Api.AddComments productId comment) = 
    Core.AddComments productId comment

-- | Transformer function to convert Core type to Api type
convertAddProductToCore :: Api.AddProduct -> Core.AddProduct
convertAddProductToCore (Api.AddProduct name category price currency description status) = Core.AddProduct name category price currency description status

-- | convert Core ProductCart type to Api ProductCart type
convertViewCartCoreToApi :: Core.ProductCart ->  Api.ProductCart
convertViewCartCoreToApi (Core.ProductCart productname
                                    price
                                    currency
                                    availability
                                    description
                                    cartquantity
                                    updatedat ) =
                                                Api.ProductCart
                                                        productname
                                                        price
                                                        currency
                                                        availability
                                                        description
                                                        cartquantity
                                                        updatedat


convertTrackingApiToCore :: Api.Tracking -> Core.Tracking
convertTrackingApiToCore (Api.Tracking orderid refNo date ) =
    Core.Tracking orderid refNo date
convertToFinalTypeForAddProduct :: Core.AddProduct-> Int -> DT.AddProductFinalType
convertToFinalTypeForAddProduct (Core.AddProduct name _ price currency description status) categoryId  =
          AddProductFinalType name categoryId price currency description status


-- | Transformer function to convert the core type to final type for addproduct endpoint
convertToFinalTypeForEditProduct :: Api.EditProduct-> Int  -> Core.EditProduct
convertToFinalTypeForEditProduct (Api.EditProduct id name _ price currency description status) categoryId =
         Core.EditProduct  name categoryId price currency description status id

convertApiOrderStatusToCore ::Api.AcceptOrReject ->Core.AcceptOrReject
convertApiOrderStatusToCore Api.Accept =Core.Accept
convertApiOrderStatusToCore Api.Reject =Core.Reject

-- | Transformer function to convert the core type to final type for temporarypricechange endpoint
convertTempAPiToCore :: Api.AddTempValueInfo -> Core.AddTempValueInfo'
convertTempAPiToCore (Api.AddTempValueInfo
                          tempId tempValue
                          tempCurrency
                          tempstartDate
                          tempEndDate) =
                      Core.AddTempValueInfo'
                         tempId
                         tempValue
                         tempCurrency
                         tempstartDate
                         tempEndDate

-- | Convert Core type wishlist data in to Api type
convertWishlistCoreToApi :: Core.WishList -> Api.WishList
convertWishlistCoreToApi (Core.WishList productName
                                        productPrice
                                        currency
                                        productAvailability) = Api.WishList productName
                                                                    productPrice
                                                                    currency
                                                                    productAvailability
