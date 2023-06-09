{-# LANGUAGE OverloadedStrings #-}
module ApiSpec where

import API
import VisionApi.Types
import Servant
import qualified Network.Wai.Handler.Warp         as Warp
import MockDb 
import MockAPI
import VisionApi.Config
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import VisionApi.Logger.Internal
import VisionApi.Logger.Types
import VisionApi.Logger
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BS



mockConfig = Configuration{
                            dbName="mockDb"
                          , dbUser="mockUser"
                          , dbPass="mockPass"
                          , dbHost="mockHost"
                          , logType=[Console]
                          , logLevel=DEBUG
                          }



testApp::Application
testApp = do
    let logger = runLog(getLogger mockConfig)
    let mockDb = MockDb
    let mockAPI = MockAPI
    let env = Env {
                    envDb=mockDb
                  , envConfig = mockConfig
                  , envLogger = logger
                  , envObjDtct = mockAPI
                  }
    serve api $hoistServer api (nt env) server

encodedString = "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"

instance (FromJSON a)=>FromJSON (ApiResponse a)

businessLogicSpec :: Spec
businessLogicSpec =
   with (return testApp) $ do

-- test suite for the endpoint GET /images
    describe "GET /images" $ do
      it "should get all images with metadata" $do
        get "/images" `shouldRespondWith` 200 {matchBody = MatchBody getImageResponseMatcher}
    
-- test suite for the endpoint GET /images/id
    describe "Get /images/id" $ do
      it "should get metadata of image with id 1" $ do
        get "/images/1" `shouldRespondWith` 200 {matchBody = MatchBody getImageResponseMatcherId1}
      it "should get metadata of image with id 2" $ do
        get "/images/2" `shouldRespondWith` 200 {matchBody = MatchBody getImageResponseMatcherId2}
      it "could not parse id since a char is given" $ do
        get "/images/b" `shouldRespondWith` "could not parse: `b' (input does not start with a digit)" {matchStatus = 400}
      it "could not parse since a special char is given" $ do
        get "/images/@" `shouldRespondWith` "could not parse: `@' (input does not start with a digit)" {matchStatus = 400}
      it "id out of scope" $ do
        get "/images/5" `shouldRespondWith` "{\"error\":\"Server Error : Unable to fetch image with imageId :5, Try again\",\"result\":null,\"code\":500}" {matchStatus = 500}
    
-- test suite for the endpoint GET /images?object=tags  
    describe "GET /images?object=tags" $ do
      it "should get image with metadata correspond to the single tag" $do
        get "/images?objects=mountain" `shouldRespondWith` 200 {matchBody = MatchBody getImageResponseMatcherTag1}
      it "should get images with metadata correspond to the multiple tags given" $do
        get "/images?objects=tree,mountain" `shouldRespondWith` 200 {matchBody = MatchBody getImageResponseMatcherTag2}
      it "should throw error message(tag not in db)" $do
        get "/images?objects=pen" `shouldRespondWith` "{\"error\":\"Server Error : Unable to fetch images with given object. Try again\",\"result\":null,\"code\":500}" {matchStatus = 500}
      it "should throw error message(tag is given as integer)" $do
        get "/images?objects=45" `shouldRespondWith` "{\"error\":\"Invalid input:45 Objects must be comma separated string\",\"result\":null,\"code\":400}"  {matchStatus = 400} 
      it "should throw error message(no tags given)" $do
        get "/images?objects=" `shouldRespondWith` "{\"error\":\"Invalid input: Objects must be comma separated string\",\"result\":null,\"code\":400}" {matchStatus = 400} 
    




getImageResponseMatcher _ body = case (decode body::Maybe (ApiResponse [ImageDetails])) of
                                   Just val -> if (VisionApi.Types.result val) == getAllImagesResponse 
                                                  then Nothing
                                                  else Just "Response body mismatch"
                                   Nothing  -> Just "Invalid Response Schema. Failed to Parse"

getImageResponseMatcherId1 _ body = case (decode body::Maybe (ApiResponse ImageDetails)) of
                                   Just val -> if (VisionApi.Types.result val) == (getImageByIdResponse 1)
                                                  then Nothing
                                                  else Just "Response body mismatch"
                                   Nothing  -> Just "Invalid Response Schema. Failed to Parse"

getImageResponseMatcherId2 _ body = case (decode body::Maybe (ApiResponse ImageDetails)) of
                                   Just val -> if (VisionApi.Types.result val) == (getImageByIdResponse 2)
                                                  then Nothing
                                                  else Just "Response body mismatch"
                                   Nothing  -> Just "Invalid Response Schema. Failed to Parse"

getImageResponseMatcherTag1 _ body = case (decode body::Maybe (ApiResponse [ImageDetails])) of
                                   Just val -> if (VisionApi.Types.result val) == (getImagesByTagResponse "mountain")
                                                  then Nothing
                                                  else Just "Response body mismatch"
                                   Nothing  -> Just "Invalid Response Schema. Failed to Parse"

getImageResponseMatcherTag2 _ body = case (decode body::Maybe (ApiResponse [ImageDetails])) of
                                   Just val -> if (VisionApi.Types.result val) == (getImagesByTagResponse "tree")
                                                  then Nothing
                                                  else Just "Response body mismatch"
                                   Nothing  -> Just "Invalid Response Schema. Failed to Parse"

-- getUploadResponseMatcher _ body  = case  (decode body::Maybe ImageDetails) of
--                                    Just val -> if val == (getUploadResponse)
--                                                   then Nothing
--                                                   else Just "Response body mismatch"
--                                    Nothing  -> Just "Invalid Response Schema. Failed to Parse"
