main = do
   let outfile = "output.txt"
   writeFile outfile "Hello World \n"
   appendFile outfile "Hi World"