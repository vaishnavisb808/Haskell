import qualified Data.Map as Map
import System.IO ()
import Data.Char
data Spice = Red_Chilli | Turmuric | Corainder | Pepper | Chicken_Masala deriving (Show, Ord, Eq)

type Price = Double
type Quantity = Int
type Total = Double
type NeedtoOrder = Bool
data SpiceData = SpiceData {
                 quantity :: Quantity,
                 price:: Price,
                 total:: Total,
                 needtoorder :: NeedtoOrder
                 } deriving Show

spices :: [Spice]
spices = [Red_Chilli,Turmuric,Corainder ,Pepper,Chicken_Masala]

values :: [(Int,Int)]
values = [(10,1.0),(20,2.5),(30,3.5),(40,4.5),(0,5.5)]

spicecatalog :: Map.Map Spice (Int, Int)
spicecatalog = Map.fromList (zip spices values)

getSpiceData:: Spice -> Map.Map Spice(Int, Int)->Maybe(Int,Int)
getSpiceData spice spicecatalog = Map.lookup spice spicecatalog

main::IO()
main = do
  let spiceDataList = map(`getSpiceData` spicecatalog)spices
  let spiceDataList' = map (\(Just qtyAndPrice)->qtyAndPrice)spiceDataList
  let spiceinfo = checkNeedToOrder(calculateLogistics spiceDataList')
  let outfile = "output.txt"
  writeFile outfile ""
  appendFile outfile " | Spice | Qty | Price | Total | Should Order\n"
  mapM_((appendFile outfile).getFormattedTable)(zip spices spiceinfo)

calculateLogistics::[(Quantity,Price)]->[(Quantity,Price,Total)]
calculateLogistics spiceDataList = map (\d->(fst d ,snd d , fromIntegral(fst d)*snd d))spiceDataList

checkNeedToOrder::[(Quantity,Price,Total)]->[SpiceData]
checkNeedToOrder spiceDataList = map checkQuantity spiceDataList
                                  where checkQuantity = (\(q,p,t)->if q<10 then SpiceData q p t True else SpiceData q p t False)

getFormattedTable::(Spice,SpiceData)->String
getFormattedTable rowData = row
                          where row = " | "++show(spice) ++ " | "++show(quantity spiceinfo)++" | "++show(price spiceinfo)++" | "++show(total spiceinfo)++" | "++show(needtoorder spiceinfo)++"\n"
                                spice = fst rowData
                                spiceinfo = snd rowData