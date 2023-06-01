myHead ::[a] ->Maybe a
myHead []= Nothing
myHead (x:xs) = Just x

eitherFunc :: String  ->Either  String Int
eitherFunc "" = Left  "String"
eitherFunc str = Right $ length str

main :: IO()
main = do
str <- getLine
let x = eitherFunc str
print x

