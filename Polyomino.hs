module Polyomino where
import Sort;

-- define Polyomino as a set of points in R2
newtype Polyomino = Polyomino [(Int,Int)] deriving (Show, Eq)

-- soft Polyomino lexicographically
sortPolyomino :: Polyomino -> Polyomino
sortPolyomino (Polyomino pts) = Polyomino (mergeSort pts)

-- generate grid of points in R2
genGrid :: Int -> Int -> Int -> Int -> [(Int,Int)]
genGrid xMin xMax yMin yMax | xMax == xMin = map (xMin,) [yMin..yMax]
                            | otherwise    = map (xMin,) [yMin..yMax] ++ genGrid (xMin + 1) xMax yMin yMax

-- sorts list of points into a list of lists of points according to their y-coordinate
sortByRow :: [[(Int,Int)]] -> (Int,Int) -> [[(Int,Int)]]
sortByRow [] _ = []
sortByRow lsts pt | not (any (\lst -> snd (head lst) == snd pt) lsts) = lsts++[[pt]]
                  | otherwise                                         = map (\lst -> if snd (head lst) == snd pt then lst++[pt] else lst) lsts

-- sorts grid into rows
gridToRows :: [(Int,Int)] -> [[(Int,Int)]]
gridToRows [] = [[]]
gridToRows pts = foldl sortByRow [[head pts]] (tail pts)

parsePolyomino :: Polyomino -> (Int,Int) -> Char
parsePolyomino (Polyomino pts) pt = if pt `elem` pts then 'o' else ' '

parsePolyominoRow :: Polyomino -> [(Int,Int)] -> String
parsePolyominoRow pol = map (parsePolyomino pol)

printPolyomino :: Polyomino -> IO ()
printPolyomino (Polyomino pts) = putStrLn $ concatMap ((++"\n") . parsePolyominoRow (Polyomino pts)) (gridToRows (genGrid xMin xMax yMin yMax))
                                 where xMin = minimum (map fst pts)
                                       xMax = maximum (map fst pts)
                                       yMin = minimum (map snd pts)
                                       yMax = maximum (map snd pts)

printPolyominos :: [Polyomino] -> IO ()
printPolyominos = mapM_ printPolyomino

parsePolyominos :: [[(Int,Int)]] -> (Polyomino, [Polyomino])
parsePolyominos []     = (Polyomino [], [])
parsePolyominos (x:xs) = (Polyomino x, map Polyomino xs)

