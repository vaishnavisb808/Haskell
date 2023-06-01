{-|
    Module : ShopperAPI.Core.Utils
    Description : Functions to produce json formatted response body along with
                  appropriate status code for error messages
-}
{-# LANGUAGE OverloadedStrings #-}

module  ShopperAPI.Core.Utils where


import           Crypto.BCrypt            (hashPasswordUsingPolicy,
                                           slowerBcryptHashingPolicy)
import           Data.Aeson               (KeyValue ((.=)), Value (Null),
                                           encode, object,Value (Null), encode, object, (.=))
import           Data.ByteString.Internal as BI
import           Data.ByteString.Lazy     (ByteString, fromStrict)
import qualified Data.Text                as T (pack)
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Time                (UTCTime)
import           Servant              (ServerError, err400, err401, err403,
                                       err404, err500, errBody, errHeaders)

-- | Function to encrypt password
encryptPassword :: String -> IO(Maybe BI.ByteString)
encryptPassword password = hashPasswordUsingPolicy slowerBcryptHashingPolicy (packChars password)

{-| Helper function to produce json formatted response body along with
    appropriate status code for error messages
-}
body code msg= object ["code".=(code::Int),"error".=msg,"result".=Null]

-- |400 : Bad Request.The server can't return a response due to an error on the client's end
jsonError400::String->ServerError
jsonError400  msg = err400{errBody=encode (body 400 msg)
                    , errHeaders = [("Content-Type", "application/json")]
                    }

{-| 401 : This is returned by the server when the target resource lacks valid authentication
    credentials. -}
jsonError401::String->ServerError
jsonError401  msg = err401{ errBody=encode (body 401 msg)
                    , errHeaders = [("Content-Type", "application/json")]
                    }

{-| 403 : The requested resource was access denied.-}
jsonError403::String->ServerError
jsonError403  msg = err403{ errBody=encode (body 403 msg)
                    , errHeaders = [("Content-Type", "application/json")]
                    }

{-| 404 : The requested resource was not found. The requested resource does not exist, and the
   server does not know if it ever existed -}
jsonError404::String->ServerError
jsonError404  msg = err404{ errBody=encode (body 404 msg)
                    , errHeaders = [("Content-Type", "application/json")]
                    }

-- | 500 : Something went wrong on the server and the requested resource was not delivered
jsonError500::String->ServerError
jsonError500  msg = err500{ errBody=encode (body 500 msg)
                , errHeaders = [("Content-Type", "application/json")]
                }