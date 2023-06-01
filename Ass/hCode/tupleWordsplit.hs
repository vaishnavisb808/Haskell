tupleFromstr ::String ->Maybe (String, String, Int)
tupleFromstr inp = if length alltokens /=3
                     then Nothing
                     else Just (alltokens !! 0, alltokens !! 1,age)
                     where 
                         alltokens =words inp
                         age = (read(alltokens !! 2):: Int)
data Person =Person{
                     fname :: String,
                     lname :: String,
                     age ::Int
}deriving Show

personFromTup :: (String, String, Int) -> Person
personFromTup (fname,lname,age) = Person fname lname age