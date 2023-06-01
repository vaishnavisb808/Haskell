module CryptoPlus where
import Crypto ( parseInput, pipeFormat, decrypt, encrypt', encrypt)
import Util ( splitBy )
import System.Environment


decryptMessage offset encryptedString= do
                                        let correctedInput = map (\x -> x-offset) (parseInput encryptedString)
                                        let correctedInputString =  foldr1 pipeFormat (map show correctedInput)
                                        decrypt correctedInputString

encryptMessage offset plainString= do
                                        let encryptedList = encrypt' plainString
                                        let correctedList = map (+offset) encryptedList 
                                        let correctedString | null correctedList = ""
                                                            | otherwise = foldr1 pipeFormat (map show correctedList)
                                        return correctedString