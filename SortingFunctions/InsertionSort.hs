

insertionSort ::(Ord a) => [a] -> [a]
insertionSort list = sort list []
                where
                    sort ::(Ord a) => [a] ->[a] -> [a]
                    sort [] acc = acc
                    sort (x:xs) acc = sort xs (insert x acc)

insert ::(Ord a) => a -> [a] -> [a]
insert value [] = [value]
insert value [single]
                | value < single = [value,single]
                | otherwise = [single,value]
insert value list@(x:xs)
                | value < x  = value:list
                | otherwise = x:(insert value xs)
