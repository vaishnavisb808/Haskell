module ExTrans where
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
-- | getting name and checking it empty or not then return maybe type
askforName:: String->MaybeT IO String
askforName str= MaybeT $do
        if length(str) > 0
        then return $ Just str
        else return Nothing
-- |getting age and checking then return maybe type
askforAge:: String->MaybeT IO String
askforAge age = MaybeT $do
        if length(age) >0
        then return $ Just age
        else return Nothing
-- |main function call the name ,age, and example function with arg to perform corresponding action
mains :: IO ()
mains= do
    example'<- runMaybeT $ do
        nam<- askforName "vaish"
        ag<- askforAge "22"
        return (nam,ag)
    case example' of
        Nothing -> print "error"
        Just (n,a) ->print example'