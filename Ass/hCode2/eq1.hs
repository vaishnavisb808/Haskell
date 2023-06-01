data DateOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show,Eq)
instance Ord DateOfWeek where
  compare Fri Fri=EQ
  compare Fri _ =GT
  compare  _ Fri=LT
  compare  _ _ =EQ