module Matrix where

newtype Matrix = Matrix [[Bool]] deriving Show

printRow :: [Bool] -> String
printRow = map (\x -> if x then '1' else '0')

printMat :: Matrix -> IO ()
printMat (Matrix rows) = putStr $ concatMap (\row -> printRow row ++ "\n") rows

-- Getters -- 

getCol :: Matrix -> Int -> [Bool]
getCol (Matrix rows) n = map (!! n) rows

-- Basically transposes the matrix
getCols :: Matrix -> [[Bool]]
getCols (Matrix rows) = map (getCol (Matrix rows)) [0..(length (head rows) - 1)]

getEl :: Matrix -> Int -> Int -> Bool
getEl (Matrix rows) n m = (rows !! n) !! m

rowLength :: Matrix -> Int
rowLength (Matrix rows) = length (head rows)

colLength :: Matrix -> Int
colLength (Matrix rows) = length rows
    
-- Removing utils --

removeEl :: Int -> [a] -> [a]
removeEl n lst = fst splitted ++ tail (snd splitted)
                 where splitted = splitAt n lst

removeRow :: Matrix -> Int -> Matrix
removeRow (Matrix rows) n = Matrix (removeEl n rows)

removeRows :: Matrix -> [Int] -> Matrix
removeRows mat [] = mat
removeRows mat [x] = removeRow mat x
removeRows mat (x:xs) = removeRows (removeRow mat x) (map (\x -> x-1) xs)

removeCol :: Matrix -> Int -> Matrix
removeCol (Matrix rows) n = Matrix (map (removeEl n) rows)

removeCols :: Matrix -> [Int] -> Matrix
removeCols mat [] = mat
removeCols mat [x] = removeCol mat x
removeCols mat (x:xs) = removeCols (removeCol mat x) (map (\x -> x-1) xs)

removeRowsAndCols :: Matrix -> [Int] -> [Int] -> Matrix
removeRowsAndCols mat cols rows = removeCols (removeRows mat rows) cols

removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x:xs) = x: removeDuplicates [k | k <- xs, k /= x]

isEmpty :: Matrix -> Bool
isEmpty (Matrix []) = True
isEmpty _ = False

countTruesInCols :: Matrix -> [Int]
countTruesInCols (Matrix rows) = map (length . filter id) (getCols (Matrix rows))


filterFewest :: [Int] -> [Bool]
filterFewest lst = map (== fewest) lst
                   where fewest = minimum lst

chooseColStep1Helper :: Matrix -> Int -> Int
chooseColStep1Helper (Matrix rows) n | n == length (head rows) = -1 -- Edgecase if function receives matrix with a purely False column (shouldn't happen)
                                     | fewest !! n = n
                                     | otherwise   = chooseColStep1Helper (Matrix rows) (n+1)
                              where fewest = filterFewest (countTruesInCols (Matrix rows))

hasEmptyCol :: Matrix -> Bool
hasEmptyCol mat = chooseColStep1 mat == 0

hasTrue :: [Bool] -> Bool
hasTrue lst = not (null lst)

-- Outputs indexex of columns C such that for given row R M_(R,C) == True
findTrueCols :: Matrix -> Int -> [Int]
findTrueCols (Matrix rows) selRowId = [k | k <- [0..(length (head rows) - 1)], getEl (Matrix rows) selRowId k]

-- ALGORITHM --

-- Outputs the index of column with fewest 1's (Trues)
chooseColStep1 :: Matrix -> Int
chooseColStep1 mat = chooseColStep1Helper mat 0

-- Outputs indexex of rows R such that for given column C M_(R,C) == True
chooseRowsStep2 :: Matrix -> Int -> [Int]
chooseRowsStep2 (Matrix rows) selColId = [k | k <- [0..(length rows - 1)], getEl (Matrix rows) k selColId]

chooseSubColsStep3_1 :: Matrix -> Int -> [Int]
chooseSubColsStep3_1 mat r = [j | j <- [1..]]

chooseSubRowsStep3_2 :: Matrix -> Int -> [Int]
-- chooseSubRowsStep3_2 mat j = [i | i <- []]
