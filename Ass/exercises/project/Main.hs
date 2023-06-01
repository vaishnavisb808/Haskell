import Crypt
import Final



main:: IO()
main = do
    putStrLn "enter message"
    msg <- getLine
    putStrLn "enter offset"
    ofset <- getLine
    let offset = read ofset ::Int
    let result = mEncrypt offset msg 
    print result
    --let offsetMsg = fmap (+ offset) encrypt' msg -}


   
    





