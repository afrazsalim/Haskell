import Data.List

quickSort ::(Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = let (first,second) = partition (<=x) xs
                         in quickSort first ++ [x] ++ quickSort second
