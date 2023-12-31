module KnuthMatrixGeneration where
import Polyomino;
import Matrix;

-- checks if A is a subset of B
subset :: Polyomino -> Polyomino -> Bool
subset (Polyomino ptsB) (Polyomino ptsA) = not (any (`notElem` ptsB) ptsA)

-- shifts the whole polyomino so that it's arbitrary element (here head) matches given point
shiftPolToPoint :: Polyomino -> (Int,Int) -> Polyomino
shiftPolToPoint (Polyomino []) _   = Polyomino []
shiftPolToPoint (Polyomino pts) pt = Polyomino (map (\(x,y) -> (x+xDiff, y+yDiff)) pts)
                                     where xDiff = fst pt - fst (head pts)
                                           yDiff = snd pt - snd (head pts)

-- over-cover universe with given polyomino
overCover :: Polyomino -> Polyomino -> [Polyomino]
overCover (Polyomino univPts) pol = map (shiftPolToPoint pol) univPts

-- generate all copies of a polyomino pol fitting inside the universe
genFittingPols :: Polyomino -> Polyomino -> [Polyomino]
genFittingPols universe pol = filter (subset universe) (overCover universe pol)

-- apply to list
genAllFittingPols :: Polyomino -> [Polyomino] -> [Polyomino]
genAllFittingPols universe pols = map sortPolyomino (concatMap (genFittingPols universe) pols)

boolifyPol :: Polyomino -> Polyomino -> [Bool]
boolifyPol (Polyomino univPts) (Polyomino pts) = map (`elem` pts) univPts

-- for universe Polyomino and list of Polyominos generate boolian matrix
genMatrix :: Polyomino -> [Polyomino] -> Matrix 
genMatrix (Polyomino univPts) pols = Matrix (map (boolifyPol (Polyomino univPts)) (genAllFittingPols (Polyomino univPts) pols)) (length univPts)
