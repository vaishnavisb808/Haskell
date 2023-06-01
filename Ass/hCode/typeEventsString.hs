type  Events =[String]
type Prob =[Float]

data Ptable = Ptable Events Prob
createPtable ::Events ->Prob ->Ptable
createPtable e p = Ptable e normProb
   where totalprob = sum p 
         normProb = map (/ totalprob ) p

showPair :: String -> Float -> String
showPair e p = mconcat [e," ",show p ,"\n"]

instance Show Ptable where
    show (Ptable e p ) = mconcat pairs
       where pairs = zipWith showPair e p