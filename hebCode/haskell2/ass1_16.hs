import Text.Regex.Posix
prefixToname :: String -> String
prefixToname name | name =~ "*a"= ("ms"++name)
                   |otherwise =  ("mr."++name)

main :: IO()
main = do
    putStrLn "enter name"
    name <-getLine
    let name'= prefixToname name
    putStrLn name'