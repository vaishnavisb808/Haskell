module Final(mEncrypt) where
import Crypt
import Data.Maybe as Maybe ( mapMaybe )


listOfTokens encryptedString =
    Maybe.mapMaybe readInteger (extractCharacters (encrypt encryptedString))    
mEncrypt :: Int -> String  -> String 
mEncrypt offset emessage | not(null finalmessage) = 
                             foldr1 Crypt.pipeFormat finalmessage
                         | otherwise = ""   
                        where finalmessage =  map show  (fmap (+ offset) (listOfTokens emessage))


listOfToken' strig = 
     readInteger (decrypt strig)
--dInt offset emessage  = map decryptChar (fmap (subtract offset) (listOfToken' emessage))