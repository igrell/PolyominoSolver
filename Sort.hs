module Sort where

merge :: (Ord a) => [a] -> [a] -> [a]
merge x y           | null x             = y
                    | null y             = x
                    | headX <= headY   = headX : merge (tail x) y
                    | x > y              = headY : merge x (tail y)
                    where headX = head x
                          headY = head y

halveList :: (Ord a) => [a] -> ([a], [a])
halveList x = splitAt (length x `div` 2) x

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort x     | length x <= 2     = merge [head x] (tail x)
                | otherwise         = let tup = halveList x
                                      in merge (mergeSort (fst tup)) (mergeSort (snd tup))
