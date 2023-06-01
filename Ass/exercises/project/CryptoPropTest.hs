import Test.QuickCheck
import Crypt 
import Test.QuickCheck.Property (Property)

acceptedCharecters :: [Char]
acceptedCharecters = ['a' .. 'b']++['A' .. 'Z']++[' ']

inverse :: String -> Property
inverse xs=all (`elem` inputCharacters) xs ==> (decrypt.encrypt) xs== xs

main = quickCheck inverse
