isodd :: Int -> Bool
isodd x
        | rem x 2/=0 =True
		| otherwise =False
iseven :: Int -> Bool
iseven x
        | rem x 2==0 =True
		| otherwise =False
evenORodd :: Int -> String
evenORodd x
          |iseven(x)="even number"
		  |isodd(x)="odd number"