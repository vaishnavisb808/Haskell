import SpiceFile 
userStockCheck:: Spice -> String
userStockCheck spice |spice `elem` spices && Quantity = putStrLn"stock not availabe"
               |otherwise= putStrLn"stock available"
uTotal=uQuantity * Price
putStrLn"amount to pay"++uTotal
main:: IO()
main = do
putStrLn "enter spice you need"
uSpice <- getLine   
let avail=userStockCheck uSpice
putStrLn "enter the quantity needed"
uQuantity<- getLine
let uPrice = priceCalcu uQunatity
putStrLn "Your amount is " ++uPrice
priceCalcu:: Int ->Double
priceCalcu uQuantity| uQuantity>3 = putStrLn" ***Discount***" 
                                    tPtice = (uQuantity * Price)-30
                    |otherwise = tprice = uQuantity * Price