getCoordinates :: String -> (Int, Int, Int)
getCoordinates ('f':'o':'r':'w':'a':'r':'d':' ':x) = (read x, read x, 0)
getCoordinates ('d':'o':'w':'n':' ':x) = (0, 0, read x)
getCoordinates ('u':'p':' ':x) = (0, 0, negate (read x))
getCoordinates _ = (0, 0, 0)


addCordinates (x1, y1, z1) (x2, y2, z2) = (x1 + x2, (x2 * z1) + y2, z1 +z2)

coordinateFunc :: [String] -> (Int, Int, Int)
coordinateFunc list = do
    if null list then (0,0,0)
    else do
     let y = getCoordinates (head list)
     addCordinates y (coordinateFunc $ tail list)

main = do
    input <- fmap lines (readFile "input.txt")
    let (fst,snd,_) = coordinateFunc input
    print $ fst * snd