import GHC.Show (Show)
data Underage = Underage [Char] deriving (Eq)

instance Show Underage where 
    show (Underage a)="ERROR"++ a ++" more year to vote"
votingage :: Int -> Either Int  String
votingage age | age > 18 = Right " you can vote"
              | otherwise = Left (18-age)

name2reply :: String -> String
name2reply name = "Pleased to meet you," ++ name ++ ".\n"


main :: IO ()
main = do
 putStrLn " Greetings again . What is your name?"
 fstname <- getLine
 putStrLn " Enter your age"
 inpAge <- getLine
 let age = read inpAge :: Int
 let outAge = votingage age
 let d= (18-age)
 if outAge == Left (18-age) then putStrLn ("please wait" ++show d ++"more years to vote")
  else putStrLn "WOWu are eligible"



