module PolyominoPuzzleSolver where
import Polyomino;
import Matrix;
import Sort;
import KnuthMatrixGeneration;

-- KNUTH'S ALGORITHM X --
-- STEPS:
-- 0. If matrix is empty then the current solution is valid; if it has a purely 0 column, it is invalid
-- 1. Choose the first column with the smallest amount of 1s
-- 2. Choose all rows 'r' for which the chosen column 'c' has a 1
-- 3. Choose all rows and columns with 1s intersecting with those in a given row 'r' and delete them
-- 4. Reccure over reduced matrix until a path of chosen rows [r0,r1,...,rn] is found (True) or for all subbranches of chosen rows algorithm outputs false (False)


-- Outputs the index of column with fewest 1's (Trues)
chooseColStep1 :: Matrix -> Int
chooseColStep1 mat = chooseColStep1Helper mat 0

-- Outputs indexex of rows R such that for given column C M_(R,C) == True
chooseRowsStep2 :: Matrix -> Int -> [Int]
chooseRowsStep2 mat c = [row | row <- [0..(colLength mat - 1)], getEl mat row c]

chooseSubColsStep3_1 :: Matrix -> Int -> [Int]
chooseSubColsStep3_1 mat r = [j | j <- [0..(rowLength mat - 1)], getEl mat r j]

chooseSubRowsStep3_2 :: Matrix -> Int -> [Int]
chooseSubRowsStep3_2 mat j = [i | i <- [0..(colLength mat - 1)], getEl mat i j]

combineSubRowsStep3_2_5 :: Matrix -> Int -> [Int]
combineSubRowsStep3_2_5 mat r = mergeSort $ removeDuplicates $ foldl (\subrows j -> subrows ++ chooseSubRowsStep3_2 mat j) [] (chooseSubColsStep3_1 mat r)

deleteSubRowsStep3_3 :: Matrix -> [Int] -> Matrix
deleteSubRowsStep3_3 mat [] = mat
deleteSubRowsStep3_3 mat [r] = removeRow mat r
deleteSubRowsStep3_3 mat (r:rs) = deleteSubRowsStep3_3 (removeRow mat r) (map (\x -> x-1) rs)

deleteSubColsStep3_4 :: Matrix -> [Int] -> Matrix
deleteSubColsStep3_4 mat [] = mat
deleteSubColsStep3_4 mat [c] = removeCol mat c
deleteSubColsStep3_4 mat (c:cs) = deleteSubColsStep3_4 (removeCol mat c) (map (\x -> x-1) cs)

reduceMatrixStep3 :: Matrix -> Int -> Matrix
reduceMatrixStep3 mat r = deleteSubColsStep3_4 (deleteSubRowsStep3_3 mat (combineSubRowsStep3_2_5 mat r)) subcols
                        where subcols = chooseSubColsStep3_1 mat r

-- returns all matrices for subalgorithms
knuthStep :: Matrix -> [Matrix]
knuthStep mat = map (reduceMatrixStep3 mat) (chooseRowsStep2 mat (chooseColStep1 mat))

mapToSols :: Matrix -> [Bool]
mapToSols mat = map solvePuzzle (knuthStep mat)

solvePuzzle :: Matrix -> Bool
solvePuzzle mat | isEmpty mat = True
                | hasEmptyCol mat || overcoverEdgecase mat = False
                | otherwise   = or (mapToSols mat)
