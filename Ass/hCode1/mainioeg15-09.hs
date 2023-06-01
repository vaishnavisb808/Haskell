name2reply :: String -> String
name2reply name =
             "pleased to meet you "++name ++"\n"++ "your name contains "++ charcount ++" characters"
             where charcount = show (length name)

getFullname:: String -> String -> String
getFullname fName lname =  "Your first name is  "++fName++" Your last name is  "++lname++"\n"

ageTobool :: Int -> Bool
ageTobool age |age >= 18 = True
                |otherwise =False
main :: IO()
main = do
      putStrLn " greetings \n what is your name?"
      inpr<- getLine
      let outr = name2reply inpr
      putStrLn outr
      putStrLn " enter first name"
      fname<- getLine
      putStrLn " enter second name"
      lname<- getLine
      let outfn= getFullname fname lname
      putStrLn outfn
      putStrLn " Your age"
      agel<- getLine
      let agec =read agel :: Int
      let outAge = ageTobool agec
      if outAge == True then putStrLn " You can vote"
      else putStrLn " not eligible"