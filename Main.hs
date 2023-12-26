import Polyomino;
import Matrix;
import KnuthMatrixGeneration;
import PolyominoPuzzleSolver;
import System.IO;
import System.Environment;

-- Testing
--- Polyominos
pol1 = Polyomino [(0,0)]
pol2 = Polyomino [(0,0),(0,1)]
pol3 = Polyomino [(0,0),(0,1),(1,0)]
pol4 = Polyomino [(0,0),(0,1),(0,2),(0,3),(1,2)]
pol5 = Polyomino [(0,0),(0,1),(0,2),(0,3),(1,2),(2,2),(2,1),(2,0)]
pol6 = Polyomino [(1,0),(1,1),(0,1),(1,2)]
pol7 = Polyomino [(0,0),(1,0)]
--- Universes
univ1 = Polyomino [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
univ2 = Polyomino [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

contentToWords :: String -> [[String]]
contentToWords contents = map words (lines contents)

-- Converts list of words to suitable list of points
wordsToPoints :: [String] -> [(Int,Int)]
wordsToPoints contentWords = map read contentWords :: [(Int,Int)]

contentToList :: String -> [[(Int,Int)]]
contentToList contents = map wordsToPoints (contentToWords contents)

contentToMatrix :: String -> Matrix
contentToMatrix contents = uncurry genMatrix pols
                           where pols = parsePolyominos (contentToList contents)

main = do
--     (fstArg:_) <- getArgs
    fileContents <- readFile "input.txt"
    print (solvePuzzle (contentToMatrix fileContents))
