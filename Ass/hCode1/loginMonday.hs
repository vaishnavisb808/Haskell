main1 :: IO ()
main1 = do
  maybeUserName <- readUserName
  case maybeUserName of
    Nothing -> print "Invalid user name!"
    Just (uName) -> do
      maybeEmail <- readEmail
      case maybeEmail of
        Nothing -> print "Invalid email!"
        Just (email) -> do
          maybePassword <- readPassword
          Case maybePassword of 
                 Nothing -> print "Invalid Password"
                 Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
  putStrLn "Please enter your username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail :: IO (Maybe String)
readEmail = do
  putStrLn "Please enter your email!"
  str <- getLine
  if '@' `elem` str && '.' `elem` str
    then return $ Just str
    else return Nothing

readPassword :: IO (Maybe String)
readPassword = do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || null (filter isUpper str) || null (filter isLower str)
    then return Nothing
    else return $ Just str

login :: String -> String -> String -> IO ()
...