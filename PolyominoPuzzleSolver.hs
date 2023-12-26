module PolyominoPuzzleSolver where
import Polyomino;
import Matrix;
import KnuthMatrixGeneration;

chooseRows :: Matrix -> [Int]
chooseRows mat = chooseRowsStep2 mat (chooseCol mat)
                where chooseCol = chooseColStep1

getIdsToRemove :: Matrix -> Int -> ([Int], [Int])
getIdsToRemove mat rowId = (chosenCols,removeDuplicates $ foldl (\colsLst colId -> colsLst ++ chooseRowsStep2 mat colId) [] chosenCols) -- TODO chooseRowsStep2 dobrze??
                         where chosenCols = findTrueCols mat rowId

deleteStep :: Matrix -> Int -> Matrix
deleteStep mat rowId = uncurry (removeRowsAndCols mat) (getIdsToRemove mat rowId)


solvePuzzle :: Matrix -> Bool
solvePuzzle mat
                | isEmpty mat = True
                | hasEmptyCol mat = False -- TODO dobrze?
                | otherwise =  not (null (map (solvePuzzle . deleteStep mat) (chooseRows mat)))
