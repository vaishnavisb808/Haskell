import qualified Data.Map as Map



data Spice = Red_Chilli | Turmeric | Garam_Masala | Meat_Masala | Corainder deriving (Show, Eq, Ord)
type SpiceData = (Int,Double)

spices :: [Spice]
spices = [Red_Chilli, Turmeric, Garam_Masala, Meat_Masala, Corainder]
		  
pq :: [SpiceData]
pq  = [(50,21.5), (60, 15.9), (55, 10.4), (45, 12.0), (40,19.9)]

spicelists = zip pq spices 
spiceCat = Map.fromList spicelists	

availableSpiceData :: [Spice]->Map.Map Spice SpiceData ->  [Maybe SpiceData]
availableSpiceData spices spiceCatalog = map (`Map.lookup` spiceCatalog) spices

isSomething :: Maybe SpiceData -> Bool
isSomething Nothing = False
isSomething (Just _) = True

filterJust::[Maybe SpiceData] -> [Maybe SpiceData]
filterJust = filter isSomething

getSpiceData:: Maybe SpiceData -> SpiceData
getSpiceData (Just spiceData) = spiceData



tquantity = [(fromIntegral fst)*snd|(fst, snd) <- pq]
maketup = zip spices tquantity



qrequired = [fst|(fst,snd) <- pq] 
chkavailability = map (\fst -> if fst < 20 then "Need  more"
                             else "Yes available") qrequired	

data Container = Pouch Spice| Lid Spice | Box Spice
instance Show Container where
   show (Pouch spice) = show spice ++ " in a pouch"
   show (Lid spice) = show spice ++ " in a lid"
   show (Box spice) = show spice ++ " in a box"

										  
data Location =  Kitchen | Cart | Food deriving Show

spiceToContainer :: Spice -> Container
spiceToContainer Red_Chilli = Pouch Red_Chilli
spiceToContainer Turmeric = Lid Turmeric
spiceToContainer spice = Box spice

placeToPut :: Container -> (Location, Container)
placeToPut (Pouch a) = (Cart, Pouch a)
placeToPut (Lid a) = (Food, Lid a)
placeToPut (Box a) = (Kitchen, Box a) 

process :: Spice -> (Location, Container)
process spice = placeToPut(spiceToContainer spice)


