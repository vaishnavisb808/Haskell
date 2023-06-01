mySqr ::[Integer]
mySqr = [x^2 | x <- [1..5]] 

myCube::[Integer]
myCube = [y^3 | y <- [1..5]] 
tup=(mySqr,myCube) 

comboTup :: [(Integer, Integer)]
comboTup=zip mySqr myCube
 
less50::[(Integer,Integer)]
less50=[(x,y)|(x,y)<-zip [x^2 | x <- [1..5]][y^3 | y <- [1..5]],x<50,y<50 ] 

findLength::Int
findLength= length([(x,y)|(x,y)<-zip [x^2 | x <- [1..5]][y^3 | y <- [1..5]],x<50,y<50])