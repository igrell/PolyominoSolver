module Matrix where

newtype Matrix = Matrix [[Bool]] deriving Show

printRow :: [Bool] -> String
printRow = map (\x -> if x then '1' else '0')

printMat :: Matrix -> IO ()
printMat (Matrix rows) = putStr $ concatMap (\row -> printRow row ++ "\n") rows

getCol :: Matrix -> Int -> [Bool]
getCol (Matrix rows) n = map (!! n) rows

removeEl :: Int -> [a] -> [a]
removeEl n lst = fst splitted ++ tail (snd splitted)
                 where splitted = splitAt n lst

removeRow :: Matrix -> Int -> Matrix
removeRow (Matrix rows) n = Matrix (removeEl n rows)

removeCol :: Matrix -> Int -> Matrix
removeCol (Matrix rows) n = Matrix (map (removeEl n) rows)

isEmpty :: Matrix -> Bool
isEmpty (Matrix []) = True
isEmpty _ = False
