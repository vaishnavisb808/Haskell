
import SpiceFile
import UserInputSpice
main:: IO()
main = do

putStrLn "enter spice you needed :"
uSpice <- getLine   
let avail=userStockCheck uSpice
putStrLn "enter the quantity needed :"
uQuantity<- getLine
let uPrice = priceCalcu uQunatity
putStrLn "Your amount is " ++uPrice