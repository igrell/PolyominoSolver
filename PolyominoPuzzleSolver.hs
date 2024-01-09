module PolyominoPuzzleSolver where
import Polyomino();
import Matrix;
import Sort;
import KnuthMatrixGeneration();

-- KNUTH'S ALGORITHM X --
-- STEPS:
-- 0. If matrix has no columns, then the current solution is valid; otherwise it is invalid
-- 1. Choose the first column with the smallest amount of 1s
-- 2. Choose all rows 'r' for which the chosen column 'c' has a 1
-- 3. Choose all rows and columns with 1s intersecting with those in a given row 'r' and delete them
-- 4. Reccure over reduced matrix until a path of chosen rows [r0,r1,...,rn] is found (True) or for all subbranches of chosen rows algorithm outputs false (False)


-- Outputs the index of column with fewest 1's (Trues)
chooseColStep1 :: Matrix -> Int
chooseColStep1 mat = chooseColStep1Helper mat 0

-- Outputs indexex of all rows R such that for given column C M_(R,C) == True
chooseRowsStep2 :: Matrix -> Int -> [Int]
chooseRowsStep2 mat c = [row | row <- [0..(colLength mat - 1)], getEl mat row c]

-- For a row R outputs all columns C s.t. M_(R,C) == True
chooseSubColsStep3_1 :: Matrix -> Int -> [Int]
chooseSubColsStep3_1 mat r = [j | j <- [0..(rowLength mat - 1)], getEl mat r j]

-- For a col J outputs all rows i s.t. M_(i,j) == True
chooseSubRowsStep3_2 :: Matrix -> Int -> [Int]
chooseSubRowsStep3_2 mat j = [i | i <- [0..(colLength mat - 1)], getEl mat i j]

-- Merge solutions from previous steps
combineSubRowsStep3_2_5 :: Matrix -> Int -> [Int]
combineSubRowsStep3_2_5 mat r = mergeSort $ removeDuplicates $ foldl (\subrows j -> subrows ++ chooseSubRowsStep3_2 mat j) [] (chooseSubColsStep3_1 mat r)

-- Delete rows
deleteSubRowsStep3_3 :: Matrix -> [Int] -> Matrix
deleteSubRowsStep3_3 mat [] = mat
deleteSubRowsStep3_3 mat [r] = removeRow mat r
deleteSubRowsStep3_3 mat (r:rs) = deleteSubRowsStep3_3 (removeRow mat r) (map (\x -> x-1) rs)

-- Delete cols
deleteSubColsStep3_4 :: Matrix -> [Int] -> Matrix
deleteSubColsStep3_4 mat [] = mat
deleteSubColsStep3_4 mat [c] = removeCol mat c
deleteSubColsStep3_4 mat (c:cs) = deleteSubColsStep3_4 (removeCol mat c) (map (\x -> x-1) cs)

-- Reduce matrix by row R (delete all cols with intersecting 1's and rows intersecting with those cols)
reduceMatrixStep3 :: Matrix -> Int -> Matrix
reduceMatrixStep3 mat r = deleteSubColsStep3_4 (deleteSubRowsStep3_3 mat (combineSubRowsStep3_2_5 mat r)) subcols
                        where subcols = chooseSubColsStep3_1 mat r

-- Returns all matrices for subbranches
knuthStep :: Matrix -> [Matrix]
knuthStep mat = map (reduceMatrixStep3 mat) (chooseRowsStep2 mat (chooseColStep1 mat))

-- Get solutions to all subbranches
mapToSols :: Matrix -> [Bool]
mapToSols mat = map solvePuzzle (knuthStep mat)

solvePuzzle :: Matrix -> Bool
solvePuzzle mat | isColless mat = True -- Stop condition (no columns)
                | otherwise   = or (mapToSols mat)
