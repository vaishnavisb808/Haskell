main ::IO()
main = do
    arg <- getLine
    let y = read arg::Int
    print y
    let y = y+1
    print y