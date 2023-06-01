countOnes pos list = length $ filter (== '1') $ map (!! pos) list

bitToDeci [] p = 0
bitToDeci (x:xs) p = val + bitToDeci xs (p+1)
                  where bit = if x == '1' then 1 else 0
                        val = bit * (2^p) 


--gammaFun ::Int-> [String] -> [String]
gammaFun 11 [] =  []
gammaFun pos list= do
    let leng=div 2 (length list)
    let len = length (head list) 
    let count = [countOnes pos list ]-- pos <- [0 .. (len -1)]]
    let finalCount = map (\c -> if(c > leng) then '1' else '0') count
    finalCount : gammaFun (pos+1) list -- | pos <- [0 .. (leng -1)]


main= do
     input<- fmap lines (readFile "input.txt")
     let mcb = gammaFun 0 input
     print mcb