sma :: [Double] -> [Double]
sma (x0:x1:xs) = (x0+x1)/2 : sma (x1:xs)
sma xs = xs

main = 
    let a ={-# SCC "list-"#-}[1..100000]
        b = {-# SCC "sma-"#-}sma a 
        c = {-# SCC "sum-"#-}sma b
        in print c

-- To compile 'ghc -fforce-recompile -O -rtsopts -prof filename.hs'
-- also use this to get .prof file 'profiling_basics28.exe +RTS -s -p' we can chang small p to caps also get 