getCoordinates :: String -> (Int, Int) 
getCoordinates ('f':'o':'r':'w':'a':'r':'d':' ':x) = (read x, 0) 
getCoordinates ('d':'o':'w':'n':' ':x) = (0, read x) 
getCoordinates ('u':'p':' ':x) = (0, negate (read x)) 
getCoordinates _ = (0, 0) 

addCordinates :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b) 
addCordinates (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

coordinateFunc :: [String] -> (Int, Int) 
coordinateFunc list = do 
    if null list 
        then (0,0) 
        else 
            do 
                let var = getCoordinates (head list) 
                addCordinates var (coordinateFunc $ tail list) 
                
main = do 
    input <- fmap lines (readFile "input.txt")  
    let x=coordinateFunc input 
    print $uncurry (*) x
    