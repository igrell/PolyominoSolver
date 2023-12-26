main = do
    text <- readFile "input.txt"
    let
        readparse txtLines = map read txtLines :: [(Int,Int)]
        parse txtLines = map words (lines txtLines)
        pcval = map readparse (parse text) 
    print pcval
    return ()
