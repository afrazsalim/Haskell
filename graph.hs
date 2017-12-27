import Data.List

--Find all possible paths (no Cycle detection)
getAllPaths ::(Eq a) => [(a,a)] -> [[a]]
getAllPaths [] = [[]]
getAllPaths (x:xs) =  findAllPath (fst x) (x:xs) ++ getAllPaths xs



findAllPath ::(Eq a) => a -> [(a,a)] -> [[a]]
findAllPath val list = path val list
            where
                (first,second) = partition((==val).fst) list
                singl = map snd first
                path val list
                            | first == [] = [[val]]
                            | otherwise = map ((:) val) $ concat $ map (\x -> findAllPath x list) singl



--selection Sort

selectionSort ::(Eq a, Ord a) => [a] -> [a]
selectionSort list = sortSelection list []


sortSelection ::(Eq a,Ord a) => [a] -> [a] -> [a]
sortSelection [] acc = acc
sortSelection list acc  = sortSelection (deleteBy (==) minNumber list) (acc ++ [minNumber])
            where
                minNumber = minimum list




--minimum spanningTree with Prim's algorithm


mkGraph = [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]


minSpanningTree :: (Eq a, Ord a) => [(a,a,Integer)] -> [(a,a,Integer)]
minSpanningTree ((f,s,c):xs) =
                           let (first,second) = partition ((==f).firstOfThree) xs
                           in findTree ((f,s,c):first) second [] []

firstOfThree :: (a,a,Integer) -> a
firstOfThree (a,_,_) = a


secondOfThree :: (a,a,Integer) -> a
secondOfThree (_,a,_) = a


test ((f,s,d):xs) = partition ((==f).firstOfThree) xs

findTree ::(Eq a,Ord a) => [(a,a,Integer)] -> [(a,a,Integer)] -> [a] -> [(a,a,Integer)] -> [(a,a,Integer)]
findTree _ [] _ acc = acc
findTree queue rest visited acc = let minimumEdge = findMinimumEdge queue visited
                                      newQueue = deleteQueu queue minimumEdge
                                      firstMin = secondOfThree minimumEdge
                                      (first,remaining) = partition((==firstMin) . firstOfThree) rest
                                   in findTree (first ++ newQueue) remaining (add visited minimumEdge) (acc ++ [minimumEdge])






add :: [a] -> (a,a,Integer) -> [a]
add list (a,b,_) = (a:b:list)


deleteQueu ::(Eq a) => [(a,a,Integer)] -> (a,a,Integer) -> [(a,a,Integer)]
deleteQueu [] _ = []
deleteQueu ((a,b,c):xs) (x,y,z)
                            | (a == x && b == y && c == z) = xs
                            | otherwise = (a,b,c) : deleteQueu xs (x,y,z)



findMinimumEdge ::(Eq a , Ord a) => [(a,a,Integer)] -> [a] -> (a,a,Integer)
findMinimumEdge queue visited
                        | existInVisted minEdge visited = findMinimumEdge (deleteQueu queue minEdge) visited
                        | otherwise = minEdge
                    where
                        minEdge = minimumBy funcs queue
                        funcs (a,b,c) (x,y,cs)
                                | c < cs = LT
                                | c > cs = GT
                                | c == cs = EQ




existInVisted ::(Eq a,Ord a) => (a,a,Integer) -> [a] -> Bool
existInVisted (a,b,c) list
                        | elem a list && elem b list = True
                        | otherwise = False
