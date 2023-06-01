module Crypt(encrypt, pipeFormat, encrypt', decrypt, parseInput,inputCharacters, extractCharacters,readInteger,decryptChar) where
import Util 
import Data.Map as Map ( Map, fromList, lookup )
import Text.Read ( readMaybe )
import Data.Maybe as Maybe ( mapMaybe )


inputCharacters :: [Char]
inputCharacters = ['a'..'z'] ++['A'..'Z']++ [' ']
encryptedCharacters :: [Int]
encryptedCharacters= [2,4..]
-- map from chars to their corresponding numbers 
encryptionMap :: Map Char Int
encryptionMap = Map.fromList (zip inputCharacters encryptedCharacters)
decryptionMap :: Map Int Char
decryptionMap = Map.fromList (zip encryptedCharacters inputCharacters)




-- ENCRYPTION
-- | return encrypted equivalent of a given character 
encryptChar :: Char -> Maybe Int
encryptChar c = Map.lookup c encryptionMap
-- | return encrypted version of input string
encrypt :: String -> String
encrypt "" = ""
encrypt inp | null encryptedList = ""
            | otherwise = foldr1 pipeFormat encryptedList
     where encryptedList = map show (Maybe.mapMaybe encryptChar inp)
encrypt' :: [Char]->[Int]
encrypt' = Maybe.mapMaybe encryptChar
pipeFormat :: [Char] -> [Char] -> [Char]
pipeFormat string1 string2 = string1 ++ "|" ++ string2



-- DECRYPTION
-- | split string by  eg: | "2|4|6|?" to [2,4,6]
extractCharacters :: String -> [String]
extractCharacters  = splitBy '|'
-- | convert [String] to [Int]
readInteger::String->Maybe Int
readInteger inpString = readMaybe inpString :: Maybe Int
decryptChar :: Int -> Maybe Char
decryptChar c = Map.lookup c decryptionMap
-- | return decrypted equivalent of input string
decrypt :: String -> [Char]
decrypt "" = ""
decrypt encryptedString = Maybe.mapMaybe decryptChar listOfTokens
                            where
                                listOfTokens =
                                    Maybe.mapMaybe readInteger (extractCharacters encryptedString)
parseInput :: String -> [Int]
parseInput inpString = Maybe.mapMaybe readInteger (extractCharacters inpString)
