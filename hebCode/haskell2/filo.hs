data DateOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq)
data Date = Date DateOfWeek Int deriving Show 

instance Eq Date where
  (==)(Date weekday dayOfMonth)
      (Date weekday1 dayOfMonth1)= 
	   weekday == weekday1
	   && dayOfMonth == dayOfMonth1
 
 

 