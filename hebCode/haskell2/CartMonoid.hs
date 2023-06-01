type  Cart =[String]
type Price =[Float]

data Ctable = Ctable Cart Price
createCtable ::Cart ->Price ->Ctable
createCtable c p = Ctable c disPrice
   where totalprice = sum p 
         disPrice = ( totalprice - 50.00 )

showPair :: String -> Float -> String
showPair c p = mconcat [c," ",show p ,"\n"]

instance Show Ctable where
    show (Ctable c p ) = mconcat $ zipWith showPair c p