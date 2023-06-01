split c [] = []
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs
examples = [('@',"pbv@dcc.fc.up.pt"), ('/',"/usr/include")]
test (c,xs) = unwords ["split", show c, show xs, "=", show ys]
    where ys = split c xs

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]
--unsplit '@' ["pbv", "dcc.fc.up.pt"] = "pbv@dcc.fc.up.pt"
--unsplit '/' ["", "usr", "include"] = "/usr/include"
main = mapM_ (putStrLn.test) examples
