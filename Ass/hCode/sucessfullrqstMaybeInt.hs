import Control.Monad.Identity(Functor)
succesfulRequest ::Maybe Int
succesfulRequest = Just 6
failedRequest :: Maybe Int
failedRequest=Nothing
incMaybe ::Maybe Int->Maybe Int
incMaybe (Just n)= Just(n+1)
incMaybe Nothing= Nothing


--succesfulStringReq :: Maybe String 
--succesfulStringReq = Just "String"
--failedStringReq :: Maybe String
--failedStringReq = Nothing
