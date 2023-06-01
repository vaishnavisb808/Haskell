
import qualified Data.Map as Map
data Spices = Chilli|Turmuric|Pepper|Coriander deriving (Show,Eq)
spices::[Spices]
spices =[Chilli,Turmuric,Pepper,Coriander]
type Quantity = [Int]
quantity=[100,100,150,200]
type Price = [Int]
price =[30,40,45,35]
ids= zip price quantity
plis= zip ids spices
olist = Map.fromList plis
data Check = Check Spices Quantity
stockAvail ::Spices ->Quantity -> Maybe Check
stockAvail spices quantity
 | quantity <= 20 Just $ Check spices quantity
 | otherwise = Nothing

 data Package = Pack Spices|Bottle Spices | Bag Spices
instance Show Package where
    show (Pack spices) = show spices ++"in a pack"
    show (Bottle spices ) =show spices ++"in a bottle"
    show (Bag spices) = show spices ++"in a bag"

spicesToContainer :: Spices -> Package
spicesToContainer Pepper = Bag Pepper
spicesToContainer Coriander = Bottle Coriander
spicesToContainer spices = Pack spices
totalPrice :: Quantity -> Price ->String
totalPrics quantity price
tprice= quantity * price
show tprice