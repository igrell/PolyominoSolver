import Polyomino;
import Matrix;
import KnuthMatrixGeneration;
import PolyominoPuzzleSolver;
import System.IO;
import System.Environment;

-- TESTING -- 
--- Polyominos
pol1 = Polyomino [(0,0)]
pol2 = Polyomino [(0,0),(0,1)]
pol3 = Polyomino [(0,0),(0,1),(1,0)]
pol4 = Polyomino [(0,0),(0,1),(0,2),(0,3),(1,2)]
pol5 = Polyomino [(0,0),(0,1),(0,2),(0,3),(1,2),(2,2),(2,1),(2,0)]
pol6 = Polyomino [(1,0),(1,1),(0,1),(1,2)]
pol7 = Polyomino [(0,0),(1,0)]
pol8 = Polyomino [(0,1),(1,0),(1,1)]
pol9 = Polyomino [(0,0),(1,0),(2,0)]
--- Universes
univ1 = Polyomino [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
univ2 = Polyomino [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
univ3 = Polyomino [(0,0),(0,1),(1,0),(1,1)]

-- Example from wikipedia
mat1 = Matrix [[True,False,False,True,False,False,True],[True,False,False,True,False,False,False],[False,False,False,True,True,False,True],[False,False,True,False,True,True,False],[False,True,True,False,False,True,True],[False,True,False,False,False,False,True]]
mat2 = genMatrix univ3 [pol3,pol8]
mat3 = genMatrix univ3 [pol1]
mat4 = Matrix [[True,True,False],[False,True,True]]
mat5 = genMatrix univ1 [pol3,pol7,pol6]
mat6 = genMatrix univ1 [pol3,pol6,pol7]

-- \TESTING --

contentToWords :: String -> [[String]]
contentToWords contents = map words (lines contents)

-- Converts list of words to suitable list of points
wordsToPoints :: [String] -> [(Int,Int)]
wordsToPoints contentWords = map read contentWords :: [(Int,Int)]

contentToList :: String -> [[(Int,Int)]]
contentToList contents = map wordsToPoints (contentToWords contents)

polsToMatrix :: (Polyomino,[Polyomino]) -> Matrix
polsToMatrix = uncurry genMatrix

-- before running the algorithm check if the matrix is empty, which can be the case for example when universe is smaller than any polyomino
solvePuzzleCond :: (Polyomino,[Polyomino]) -> Bool
solvePuzzleCond pols | isEmpty mat = False
                     | otherwise   = solvePuzzle mat
                    where mat = polsToMatrix pols

main = do
    (fstArg:_) <- getArgs
    fileContents <- readFile fstArg
    let problemPols = parsePolyominos (contentToList fileContents)
    putStrLn "Your universe:"
    printPolyomino (fst problemPols)
    putStrLn "Your polyominos:"
    printPolyominos (snd problemPols)
    putStr "Solution to the problem: "
    print (solvePuzzleCond problemPols)
