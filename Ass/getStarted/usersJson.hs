{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Crypt
import Data.Map as M
import Text.XML.HXT.Arrow.XmlState.TypeDefs (theErrorMsgList)



data Contains = Contains {
                            name :: String,
                            message :: String
                            }deriving Show

data Users = Users [Contains] deriving Show

instance FromJSON Contains where
    parseJSON (Object v) =Contains
                      <$> (v.: "name")
                      <*> (v.: "message")


-- justRemoval :: Maybe a -> a

justRemoval inp = Prelude.map(\(Just x)->x)[inp]

recursive names | (Prelude.null names) = []
                | otherwise = name (Prelude.head names) : recursive (Prelude.tail names)

recurs msgs | (Prelude.null msgs) = []
                | otherwise = message (Prelude.head msgs) : recurs (Prelude.tail msgs)
        

main :: IO()
main = do

userinp <- Prelude.getLine

file <- B.readFile "firstTask.json"

let first = decode file:: Maybe(M.Map String [Contains])
-- print (first)

let rmvJust = justRemoval first
-- print (rmvJust)

let header = Prelude.head(rmvJust)

let usersLookup = M.lookup "users" header
print (usersLookup)

let againrmvJust = justRemoval usersLookup
--print (againrmvJust)

let finalList = Prelude.head(againrmvJust)

--print (finalList)

let nameList = recursive finalList
print (nameList)

let msgList = recurs finalList
print (msgList)

let comboMap = M.fromList(Prelude.zip nameList msgList)
print (comboMap)

let result = (\(Just y)->y)(M.lookup userinp comboMap)

let decrptdResult = decrypt result

print ("message is :::: "++decrptdResult)


-- -- let dec = decode inp :: Maybe (M.Map String [Contains])

-- -- let just = Prelude.map (\(Just x)->x) [dec]