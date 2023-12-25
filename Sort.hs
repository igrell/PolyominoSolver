module Sort where

-- leqLex :: (Int,Int) -> (Int,Int) -> Bool
-- leqLex (a,b) (x,y) = (a <= x) || (a == x && b <= y)

merge :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
merge x y           | null x             = y
                    | null y             = x
                    | head x <= head y   = head x : merge (tail x) y
                    | x > y              = head y : merge x (tail y)

halveList :: [(Int,Int)] -> ([(Int,Int)], [(Int,Int)])
halveList x = splitAt (length x `div` 2) x

mergeSort :: [(Int,Int)] -> [(Int,Int)]
mergeSort x     | length x <= 2     = merge [head x] (tail x)
                | otherwise         = let tup = halveList x
                                      in merge (mergeSort (fst tup)) (mergeSort (snd tup))
