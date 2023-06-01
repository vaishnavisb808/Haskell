
countOnes pos list = length $ filter (== '1') $ map (!! pos) list

main= do
     input<- fmap lines (readFile "input.txt")
     let i = 1
     let mcb = gammaFun 0 input
    --  let f= finalFunc input i mcb
     print mcb
    --  let g= gammaFun 0 input
    --  print g
     --print (g++mostCommonBit)
    --  let ls= filterList (g++mostCommonBit) fList list (i+1)
    --  let a= removeChar ls (i+1)
    --  print a

gammaFun ::  Int->[String] ->[String]  
gammaFun pos []  =  []
gammaFun pos list= do
    let ls=[]
    let count = [countOnes pos list] 
    let leng= findLength list
    let fCount=map (\c -> if(c > leng) then '1' else '0') count
    --let f= finalFunc list fCount
    let f = filterList fcount list  ls
    --fCount
    gammaFun pos f | pos <- [0 .. (l -1)]


findLength input= do
    let leng=length input
    div leng 2
filterList :: String -> [String] -> [String] -> Int -> [String]
filterList charc [] lis j =[]
filterList charc input lis j= do

    let taker= take j $head input
    if taker==charc
        then  
            head input
            : filterList charc (tail input) lis j

        else 
            filterList charc (tail input) lis j


removeChar ::[String] ->Int ->[String]
removeChar [] j= []
removeChar (x:xs) j= drop j x : removeChar xs j
    
finalFunc input msb i= do
    -- let mcb = gammaFun 0 input
    --let i = 1
    let list=[]
    let fList= filterList msb input list i
    let res = removeChar fList i
    --finalFunc fList msb i+1
    res



