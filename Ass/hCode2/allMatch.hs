myNegate ::Bool ->Bool
myNegate True = False
myNegate False= True
-- in the first example it consider 2 values and more lines, pattern match is always work so 
--can use 2nd one is better  when we put _ it in first command not works on all condition bcz it lazy, we can use
-- it only for single charecter is better weekDay _ ="class" weekday Sat = "evaluation", here no check for sat always wrk as class

allNegate :: Bool -> Bool
allNegate x= not x