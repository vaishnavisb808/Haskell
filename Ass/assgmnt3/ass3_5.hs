filtFunc::[Integer]
filtFunc=filter(\x->(rem x 3)==0)[1..30]  

lenFun::Int
lenFun=length(filtFunc) 

remvArtcle::String->[String]
remvArtcle inpStg=filter(\x->x `notElem`["a","an","the"])(words inpStg )