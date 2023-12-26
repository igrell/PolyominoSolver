module PolyominoPuzzleSolver where
import Polyomino;
import Matrix;
import KnuthMatrixGeneration;

solvePuzzle :: Matrix -> Bool
solvePuzzle mat 
                | isEmpty mat = True
                | otherwise = False
