import qualified Data.Map as Map
alphabet= ['a'..'z']
alphaEven=[2,4..]

getZip = zip alphabet alphaEven
zipAlphaEven = Map.fromList getZip

getWithoutJust :: Maybe Int -> Int
getWithoutJust (Just e) = e
getWithoutJust Nothing = 0

charFromString :: Char -> Int
charFromString msg = getWithoutJust (Map.lookup msg zipAlphaEven )                   

main ::IO()
main= do
    putStrLn "enter the char to encrypt"
    c <- getLine
    if (length(c)/=0)
        then do
            let elem=charFromString (head c)
            print elem
            else putStrLn"end"