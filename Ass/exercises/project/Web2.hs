{-# LANGUAGE OverloadedStrings #-}
module Web2 where
import qualified Control.Monad.IO.Class as M
import Data.Aeson
import Network.HTTP.Req 
import JsonParse
import Data.Aeson.Types

--main :: IO()
main' = do
        result <- runReq defaultHttpConfig $ do
                                    r <-
                                        req
                                        GET
                                        (https "jsonkeeper.com" /: "b" /: "HBJE") 
                                        NoReqBody -- use built-in options or add your own
                                        jsonResponse -- specify how to interpret response
                                        mempty -- query params, headers, explicit port number, etc.
                                    return (responseBody r:: Object)
        --print result
        let something = parse (.: "users") result :: Result Array
        mapM_ print something 
        
        --decodeMessage result
        