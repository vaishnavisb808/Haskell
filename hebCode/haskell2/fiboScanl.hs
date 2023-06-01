fibs =1 :scanl (+) 1 fibs --if it run in prompt , it cause infinite loop, , bcz here no base condition it is a recusrsive call
fibsN x= fibs !! x -- here due to lazy evaluation it defaultly do upto 10 elements
fiby= take 20 fibs
fibfil=filter (< 100) fibs
fibTakeWhile = takeWhile(<100)fibs

