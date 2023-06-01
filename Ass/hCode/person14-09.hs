import Control.Applicative
type Name = String
type Age =Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                     |AgeTooLow deriving Show
type ValidatePerson a = Either [PersonInvalid] a

toString ::PersonInvalid->String 
toString NameEmpty ="Name is empty"
toString AgeTooLow = "Age is negative"

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /="" of
    True -> Right name
    False -> Left [NameEmpty]
mkPerson ::ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson (Right nameOkay)(Right ageOkay)= Right (Person nameOkay ageOkay)
mkPerson (Left badname)(Left badage)= Left (badname ++ badage)
mkPerson (Left badname)_ = Left badname
mkPerson _ (Left badage)= Left badage

vPerson name age = mkPerson (nameOkay name) (ageOkay age)
lvPerson name age = liftA2 Person (nameOkay name)  (ageOkay age)