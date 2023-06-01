import Text.ParserCombinators.Parsec
eol = 
    do char '\n'
       char '\r' <|> return '\n'