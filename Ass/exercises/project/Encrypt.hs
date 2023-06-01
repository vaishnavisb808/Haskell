module Crypto where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


encryptionMap=Map.fromList (zip ['a'..'z'] [2,4..])

encrypt :: String -> String
encrypt message = foldr pipeFormat "" encryptedList
 where encryptedList =map show $Maybe.mapMaybe encryptChar message

pipeFormat str1 str2 = str1 ++ "|" ++ str2

encryptChar :: Char -> Maybe Int
encryptChar c = Map.lookup c encryptionMap