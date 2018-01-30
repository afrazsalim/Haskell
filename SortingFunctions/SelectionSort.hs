import Data.List

selectionSort ::(Eq a,Ord a) => [a] -> [a]
selectionSort list = sort list []
                where sort ::(Ord a) => [a] -> [a] -> [a]
                      sort [] acc = acc
                      sort list acc = let max = maximum list
                                          newList = delete max list
                                       in sort newList (max:acc)
