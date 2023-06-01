import Text.XML.HXT.Core

main :: IO ()
main = do
  input <- readFile "nam.xml"
  to <- runX $ readString [withValidate no] input 
        //> hasName "name" 
        //> getText
  print to
