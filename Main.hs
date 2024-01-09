import Polyomino;
import Matrix;
import KnuthMatrixGeneration;
import PolyominoPuzzleSolver;
import System.IO();
import System.Environment;

-- INPUT PARSING --

contentToWords :: String -> [[String]]
contentToWords contents = map words (lines contents)

-- Converts list of words to suitable list of points
wordsToPoints :: [String] -> [(Int,Int)]
wordsToPoints contentWords = map read contentWords :: [(Int,Int)]

contentToList :: String -> [[(Int,Int)]]
contentToList contents = map wordsToPoints (contentToWords contents)

polsToMatrix :: (Polyomino,[Polyomino]) -> Matrix
polsToMatrix = uncurry genMatrix

-- BEFORE running the algorithm check if the matrix is empty, which can be the case for example when universe is smaller than any polyomino
solvePuzzleCond :: (Polyomino,[Polyomino]) -> Bool
solvePuzzleCond pols | isEmpty mat = False
                     | otherwise   = solvePuzzle mat
                    where mat = polsToMatrix pols

main :: IO ()
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
