nonsense :: Bool ->Integer
nonsense True=805
nonsense False=31337
curriedFunction:: Integer
                  ->Bool
				  ->Integer
curriedFunction i b=i+(nonsense b)
uncurriedFunction ::(Integer,Bool)
                    ->Integer
uncurriedFunction(i,b)=i+(nonsense b)
