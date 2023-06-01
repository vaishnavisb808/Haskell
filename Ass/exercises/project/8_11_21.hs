import Control.Monad.Trans.Maybe (MaybeT (MaybeT) ,runMaybeT)
import Data.String (String)
import GHC.Generics (Generic)
import GHC ()
-- example :: IO (Maybe (String, String))
-- example = do
--     ma <- askforName "name"
--     case ma of
--       Nothing -> return Nothing
--       Just a  -> do mc <- askforAge "age"
--                     case mc of
--                       Nothing -> return Nothing
--                       Just c  -> return (Just (n,c))
-- | getting name and checking it empty or notthen return
askforName:: MaybeT IO String
askforName= MaybeT $do
    putStrLn"enter name"
    str<- getLine
    if length(str) > 0
        then return $ Just str
        else return Nothing
-- |getting age and checking then return
askforAge:: MaybeT IO String
askforAge = MaybeT $do
    putStrLn"enter the age"
    age<-getLine
    if length(age) >0
        then return $ Just age
        else return Nothing
-- | if entered correct name and age then print success
example :: String -> String ->IO() 
example name age= putStrLn ("name:"++name++"\nage:"++ age)
-- |main function call the name ,age, and example function to perform corresponding action
main :: IO ()
main= do
    example'<- runMaybeT $ do
        nam<- askforName
        ag<- askforAge
        return (nam,ag)
    case example' of
        Nothing -> print "error"
        Just (n,a) -> example n a




