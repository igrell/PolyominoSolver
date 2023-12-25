import Types;
import KnuthMatrixGeneration;
import PrintPolyomino;
import System.IO;
import System.Environment;












-- Polyominos for testing
-- pol1 :: Polyomino
pol1 = Polyomino [(0,0)]
pol2 = Polyomino [(0,0),(0,1)]
pol3 = Polyomino [(0,0),(0,1),(1,0)]
pol4 = Polyomino [(0,0),(0,1),(0,2),(0,3),(1,2)]
pol5 = Polyomino [(0,0),(0,1),(0,2),(0,3),(1,2),(2,2),(2,1),(2,0)]
pol6 = Polyomino [(1,0),(1,1),(0,1),(1,2)]
pol7 = Polyomino [(0,0),(1,0)]
univ1 = Polyomino [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
univ2 = Polyomino [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

-- TODO
parseFile :: Handle -> IO [[(Int,Int)]]
parseFile fileHandle = do
    eof <- hIsEOF fileHandle
    if eof then return []
    else do
        line <- hGetLine fileHandle
        return ()
        parseFile fileHandle

main = do
    fileHandle <- openFile "input.txt" ReadMode
    parseFile fileHandle
    hClose fileHandle
