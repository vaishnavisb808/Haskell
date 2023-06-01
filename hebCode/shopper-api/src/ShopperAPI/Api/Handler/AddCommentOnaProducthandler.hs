{-|
    Module          : ShopperAPI.Api.Handler.AddCommentOnaProducthandler
    Description     : Handler for /addcomment
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module           ShopperAPI.Api.Handler.AddCommentOnaProducthandler where

import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Reader           (asks)
import           Servant                              (throwError)
import           ShopperAPI.Api.MiddleWare.Auth.Types as MT (AccessData (id))
import           ShopperAPI.Api.Types                 as AT (ApiHandler,
                                                             ApiResponse (ApiResponse),
                                                             AddComments(..))
import           ShopperAPI.Core.Types                as CT  (AddComments(productId,
                                                                           comment))
import           ShopperAPI.Core.Config.Types         (Env (envDbOps))
import           ShopperAPI.Core.Utils                (jsonError400,
                                                       jsonError500)
import           ShopperAPI.Services.Database.Types   (DbOps (addcomment))
import           ShopperAPI.Core.Transformers         (convertAddCommentsApiToCore)

-- | Handler function for addcomment endpoint.

insertComment :: AT.AddComments -> AccessData -> ApiHandler( ApiResponse String)
insertComment prdtIdAndcomment accessData= do
            let convertprdtIdAndcomment = convertAddCommentsApiToCore prdtIdAndcomment
                prdtId= CT.productId convertprdtIdAndcomment
                comment'= CT.comment convertprdtIdAndcomment
                loginid =MT.id accessData
            if prdtId > 0
                 then do
                   dbops <- asks envDbOps
                   insertInfo <- liftIO $ addcomment dbops prdtId comment' loginid
                   case insertInfo of
                      Right 1-> return $ ApiResponse "Thank you for your comment" Nothing 200
                      Left msg -> throwError $jsonError500 "insertion failed"
            else
                throwError $jsonError400 "Invalid productid"
