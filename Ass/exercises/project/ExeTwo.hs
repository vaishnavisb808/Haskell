module ExeTwo where
import Data.List(sortBy,nub)
import Data.Ord (comparing)
data User = User {
    userName::String,
    userId::Int
    } deriving (Show)
instance Eq User where
    (==) u1 u2 = userId u1 Prelude.== userId u2

task1 :: [User] -> [User]
task1 =(sortBy (comparing userId)).nub
