{-|
    Module          :  ShopperAPI.Services.Database.AddComment
    Description     :  contains function to comment on a product as a user.
-}

{-# LANGUAGE OverloadedStrings #-}

module          ShopperAPI.Services.Database.AddComment(addCommentOnaProduct) where

import           Control.Exception                  (SomeException)
import           Control.Monad.Catch                (handle)
import           Database.PostgreSQL.Simple         (Connection, connect,
                                                     connectDatabase,
                                                     connectPassword,
                                                     connectUser,
                                                     defaultConnectInfo,
                                                     execute)
import           GHC.Int                            (Int64)
import           GHC.Num                            (Integer)
import           ShopperAPI.Services.Database.Types (DbError (DbError))

-- | Funtion to add comments for a product
addCommentOnaProduct :: Int ->String ->Int ->Connection -> IO(Either DbError Int64 )
addCommentOnaProduct prdtId comment loginid conn = handle handler $ do     
        insertInfo <- execute conn "INSERT  INTO comment \  
                        \ (product_id, comments, login_id)\
                        \ VALUES (?,?,?);"( prdtId,comment,loginid )

        return $ Right insertInfo
    where
        handler :: SomeException -> IO (Either DbError Int64)
        handler er = return (Left $ DbError $ "Error while inserting comment" ++show er) 
