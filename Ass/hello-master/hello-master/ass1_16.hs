import Text.Regex.Posix
prefixToname :: String -> String
prefixToname name | name =~ "[aeiou]$"= ("Ms. "++name)
                   |otherwise =  ("Mr. "++name)

main :: IO()
main = do
    putStrLn "enter name :"
    name <-getLine
    let name'= prefixToname name
    putStrLn name'