data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
                          | NonfictionBook Nonfiction
                          deriving Show 
type AuthorName = String
-- This isn’t a sum of products, so it isn’t normal form
newtype Author = Author (AuthorName, BookType)
-- Normal form
-- Comment out previous defn of Fiction and Nonfiction
data Author' =
     Fiction AuthorName
   | Nonfiction AuthorName
   deriving (Eq, Show)
