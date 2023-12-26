module PolyominoPuzzleSolver where
import Polyomino;
import Matrix;
import KnuthMatrixGeneration;

solvePuzzle :: Polyomino -> [Polyomino] -> Bool
solvePuzzle universe pols = let knuthMat = genMatrix universe pols in
                                False
