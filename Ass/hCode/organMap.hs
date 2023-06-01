import qualified Data.Map as Map
data Organ = Heart|Brain|Kidney|Spleen|Liver deriving (Show,Eq)
organs::[Organ]
organs =[Heart,Heart,Brain,Kidney,Kidney,Liver,Spleen,Liver,Kidney,Spleen]
ids::[Int]
ids=[15,18,7,10,12,19,4,24,21,9]
organlists= zip ids organs
organCat = Map.fromList organlists
allDrawer =[1..25]
getDrawerContent:: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContent ids cat = map (\id -> Map.lookup id cat) ids 
availableOrgans = getDrawerContent allDrawer organCat
countOrgan::Organ ->[Maybe Organ] -> Int
countOrgan organ available = length (filter 
                                    (\x -> x== Just organ)
                                     available)
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _)= True

justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing=""

onlyOrgans = map showOrgan justTheOrgans
data Container = Vat Organ|Cooler Organ | Bag Organ
instance Show Container where
    show (Vat organ) = show organ ++"in a vat"
    show (Cooler organ ) =show organ ++"in a Cooler"
    show (Bag organ) = show organ ++"in a bag"
data Location = Lab|Kitchen |Toilet deriving Show
organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeToPut :: Container ->(Location,Container)
placeToPut (Vat a) = (Lab, Vat a)
placeToPut (Cooler a) = (Lab ,Cooler a)
placeToPut (Bag a) = (Kitchen,Bag a)

process :: Organ ->(Location, Container)
process organ =placeToPut (organToContainer organ)