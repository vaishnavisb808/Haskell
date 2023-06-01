data Username= Username1 String
data AcctNum= AcctNum1 Integer

data User =
        Unregiisterd String
		|Registerd Username AcctNum 
printUser :: User ->IO()
printUser (Unregiisterd name )=
                    putStrLn(" Unregiisterd User")
printUser (Registerd 
            (Username1 name)
			(AcctNum1 num)) =
			putStrLn ( name ++" "++ show num)
muser = Username1 "abc"
maccnt = AcctNum1 12345
ruser = Registerd muser maccnt
uuser = Unregiisterd "uuser"
          