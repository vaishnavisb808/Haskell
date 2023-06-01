import qualified Data.Map as Map
import System.IO
import Data.Char
type Put =String 
data Spice = Red_Chilli | Turmeric | Garam_Masala | Meat_Masala | Corainder deriving (Show, Eq, Ord)
data SpiceData = SpiceData{
    price :: Int,
    quantity :: Int
}deriving Show
spices :: [Spice]
spices = [Red_Chilli, Turmeric, Garam_Masala, Meat_Masala, Corainder]
pq :: [SpiceData]
pq  = [(50,21), (60, 15), (55, 10), (45, 12), (40,19)]
spicelists = zip pq spices
spiceCat = Map.fromList spicelists

spicyFile::[Spice]->Map.Map Spice SpiceData ->[Spice,price',quantity',total,Put]
spicyFile nspice spiceid =map (`Map.lookup` spiceid) nspice
price'=SpiceData price
quantity' = SpiceData quantity
total= price'*quantity'
if (quantity'>10)
    then Put ="no"
    else Put ="yes"

mainloop ::Handle->SpiceData ->IO()
mainloop outh =do hPutStrLn outh ("Spices\tPrice\tQuantity\tTotal\tNeed orde \n")
                 if (SpiceData==[])
                     then return ()
                     else 
                         datas= spicyFile
                          
                   mainloop outh
	
main ::IO()
main = do
       outh <- openFile "output.txt" WriteMode
       mainloop outh
       hClose outh

