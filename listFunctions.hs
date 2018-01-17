import Data.List
import Text.Read
import System.IO
import System.Directory

removeDuplicates ::(Eq a) => [(a,a,Int)] -> [(a,a,Int)]
removeDuplicates = nubBy (\(x,_,_) (a,_,_) -> if x == a
                                                then True
                                                else False)



deleteWithTuple :: (Eq a) => (a,a,Int) -> [(a,a,Int)] -> [(a,a,Int)]
deleteWithTuple value list  = deleteBy (\(p,_,_) (x,_,_) -> if p == x
                                                               then True
                                                               else False) value list



unionWithTuple ::(Eq a) => [(a,a,Int)] -> [(a,a,Int)] -> [(a,a,Int)]
unionWithTuple value list = unionBy (\(p,_,_) (x,_,_) -> if p == x
                                                           then True
                                                           else False) value list



--insertBY

minimumWithTuple :: [(a,a,Int)] -> (a,a,Int)
minimumWithTuple list = minimumBy(\(a,b,c) (x,y,z) -> if c < z
                                                     then GT
                                                     else if c > z
                                                          then LT
                                                          else EQ) list





--Reading and appending file

main2 = do
        fileName <- getLine
        withFile fileName ReadMode (\handle -> do
            contents <- hGetContents handle
            putStrLn $ "Enter the name of second file"
            secondName <- getLine
            withFile secondName AppendMode (\secondHandle -> do
                    hPutStrLn secondHandle contents
                    putStrLn $ "Done, copying file"))
