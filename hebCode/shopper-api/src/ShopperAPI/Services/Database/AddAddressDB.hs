{-|
    Module : ShopperAPI.Services.Database.AddAddressDB
    Description : Functions to insert addresses and related database operations
-}

{-# LANGUAGE OverloadedStrings #-}

module           ShopperAPI.Services.Database.AddAddressDB where

import           Control.Monad.Catch                (SomeException, handle)
import           Control.Monad.Except               (MonadIO (liftIO))
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Database.PostgreSQL.Simple         (Connection,
                                                     Only (fromOnly), query)
import           ShopperAPI.Core.Types              (Address (addressLine1, addressLine2,
                                                     cityAndState, communicationNumber, 
                                                     country, landMark, recipientFirstName,
                                                     recipientLastName, zipcode))
import           ShopperAPI.Services.Database.Types (DbError (DbError))


-- | function for retrieving a zipcode from database
zipCodeCheck :: Connection -> Integer ->IO (Either DbError Bool)
zipCodeCheck conn zipCode = handle handler $ do
                          dbResult' <- liftIO
                                          ( query conn
                                                " SELECT zipcode \
                                                \ FROM valid_zipcodes \
                                                \ WHERE zipcode=?;"
                                                [zipCode]
                                            ::IO[ Only Int]
                                          )
                          if null dbResult'
                            then do
                              return (Right False)
                            else do
                              return (Right True)
      where
          handler :: SomeException -> IO (Either DbError Bool)
          handler er = return (Left $ DbError $ "Error while fetching zipcode "
                                              ++ show er )

-- | function for inserting address into database
insertAddress :: Connection -> Address -> Int  ->IO (Either DbError Int)
insertAddress conn address userid= handle handler $ do
            addressId <- liftIO
                ( query conn
                    "INSERT INTO address \
                    \ (user_id \
                        \ ,recipient_firstname \
                        \ ,recipient_lastname \
                        \ ,address_line1 \
                        \ ,address_line2 \
                        \ ,landmark \
                        \ ,city_state \
                        \ ,zipcode \
                        \ ,country \
                        \ ,communication_number) \
                    \ VALUES (?,?,?,?,?,?,?,?,?,?) \
                    \ RETURNING address_id"
                    (userid
                      ,recipientFirstName address
                      ,recipientLastName address
                      ,addressLine1 address
                      ,addressLine2 address
                      ,landMark address
                      ,cityAndState address
                      ,zipcode address
                      ,country address
                      ,communicationNumber address)
                  :: IO [Only Int]
                )
            let addressId' = fromOnly $ Prelude.head addressId
            return (Right addressId')

  where
    handler :: SomeException -> IO (Either DbError Int)
    handler er = return (Left $ DbError $ "Error while inserting address "
                                        ++ show er )