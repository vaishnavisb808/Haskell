-- function that filter 1s in corresponding positions
countOnes pos list = length $ filter (== '1') $ map (!! pos) list

-- function that convert bit to decimal number
bitToDeci [] p = 0
bitToDeci (x:xs) p = val + bitToDeci xs (p+1)
                  where bit = if x == '1' then 1 else 0
                        val = bit * (2^p) 

main= do
     input<- fmap lines (readFile "input.txt")
     let mostCommonBit = gammaFun input
     print mostCommonBit
     let leastCommonBit= epsilonFunc mostCommonBit
     print leastCommonBit
     let gammaDeci = bitToDeci (reverse mostCommonBit) 0
     let epsilonDeci = bitToDeci (reverse leastCommonBit) 0
     print (gammaDeci * epsilonDeci)


gammaFun ::  [String] -> String
gammaFun []  =  []
gammaFun list= do
    let len = length (head list) 
    let count = [countOnes pos list | pos <- [0 .. (len -1)]]
    let finalCount = map (\c -> if(c > 500) then '1' else '0') count
    finalCount 

epsilonFunc::String ->String
epsilonFunc mostCommonBit =do
    let epsilonBit=map(\a -> if a == '1' then '0' else '1') mostCommonBit
    epsilonBit
      

   