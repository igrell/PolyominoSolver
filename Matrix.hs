module Matrix where

data Matrix = Matrix [[Bool]] Int

printRow :: [Bool] -> String
printRow = map (\x -> if x then '1' else '0')

printMat :: Matrix -> IO ()
printMat (Matrix rows colCt) = putStr $ concatMap (\row -> printRow row ++ "\n") rows

-- Getters -- 

getRow :: Matrix -> Int -> [Bool]
getRow (Matrix rows colCt) n = rows !! n

getCol :: Matrix -> Int -> [Bool]
getCol (Matrix rows colCt) n = map (!! n) rows

getColCt :: Matrix -> Int
getColCt (Matrix rows colCt) = colCt

-- Basically transposes the matrix
getCols :: Matrix -> [[Bool]]
getCols (Matrix rows colCt) = map (getCol (Matrix rows colCt)) [0..(rowLength (Matrix rows colCt) - 1)]

getEl :: Matrix -> Int -> Int -> Bool
getEl (Matrix rows colCt) rowId colId = (rows !! rowId) !! colId

rowLength :: Matrix -> Int
rowLength (Matrix rows colCt) = length (head rows)

colLength :: Matrix -> Int
colLength (Matrix rows colCt) = length rows

-- Removing utils --

removeEl :: Int -> [a] -> [a]
removeEl n lst = fst splitted ++ tail (snd splitted)
                 where splitted = splitAt n lst

removeRow :: Matrix -> Int -> Matrix
removeRow (Matrix rows colCt) n = Matrix (removeEl n rows) colCt

removeRows :: Matrix -> [Int] -> Matrix
removeRows mat [] = mat
removeRows mat [x] = removeRow mat x
removeRows mat (x:xs) = removeRows (removeRow mat x) (map (\x -> x-1) xs)

removeCol :: Matrix -> Int -> Matrix
removeCol (Matrix rows colCt) n = Matrix (map (removeEl n) rows) (colCt-1)

removeCols :: Matrix -> [Int] -> Matrix
removeCols mat [] = mat
removeCols mat [x] = removeCol mat x
removeCols mat (x:xs) = removeCols (removeCol mat x) (map (\x -> x-1) xs)

removeRowsAndCols :: Matrix -> [Int] -> [Int] -> Matrix
removeRowsAndCols mat cols rows = removeCols (removeRows mat rows) cols

removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x:xs) = x: removeDuplicates [k | k <- xs, k /= x]

-- counting Trues

countTruesInCols :: Matrix -> [Int]
countTruesInCols (Matrix rows colCt) = map (length . filter id) (getCols (Matrix rows colCt))


filterFewest :: [Int] -> [Bool]
filterFewest lst = map (== fewest) lst
                   where fewest = minimum lst

chooseColStep1Helper :: Matrix -> Int -> Int
chooseColStep1Helper (Matrix rows colCt) n | fewest !! n = n
                                     | otherwise   = chooseColStep1Helper (Matrix rows colCt) (n+1)
                              where fewest = filterFewest (countTruesInCols (Matrix rows colCt))

-- Checking properties utils

isEmpty :: Matrix -> Bool
isEmpty (Matrix [] _) = True
isEmpty (Matrix rows colCt) = maximum (map length rows) == 0 -- has only some amount of empty rows

isColless :: Matrix -> Bool
isColless (Matrix rows colCt) = colCt == 0

hasEmptyCol :: Matrix -> Bool
hasEmptyCol mat = minimum (countTruesInCols mat) == 0

hasFullCol :: Matrix -> Bool
hasFullCol mat = maximum (countTruesInCols mat) == colLength mat

hasFullRow :: Matrix -> Bool
hasFullRow (Matrix rows colCt) = any and rows
