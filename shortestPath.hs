import Data.Map as Map
import Data.List as D

--Without priority queue but priority queue can be implemented without changing much code for efficiency
data Graph k = Graph [k]  [(k,k,Integer)] deriving (Show)


mkGraph = Graph ["a","b","c","d","e","f","z"] [("a","b",1),("a","d",20),("a","e",1),("a","f",1),("b","k",1),("e","c",2),("c","d",4),("d","z",2),("d","a",10),("d","p",2),("k","d",1)]

type Edge k = (k,k,Integer)
type Node k = (k,Integer)

findShortestPath ::(Ord k , Eq k) => k -> k -> Graph k -> [k]
findShortestPath source goal (Graph nodes list) =
                                construct [goal] goal $ findPath source goal [source] (Graph nodes list) [(source,0)] (Map.empty )


construct acc goal list =
                    case (Map.lookup goal list) of
                                Nothing -> acc
                                Just (node,value) ->  construct (node:acc) node list

findPath ::(Ord k) => k -> k -> [k] -> Graph k -> [Node k] -> Map.Map k (Node k) -> Map.Map k (Node k)
findPath source goal _ _  [] adjacency = adjacency
findPath source goal visited (Graph nodes list) queue adjacency =
                                    let ((minNode,distance),newQueue) = delMin queue
                                        (matched,rest) = D.partition((==minNode).firstValue) list
                                        newAdjaceny = getAdjacency distance visited matched adjacency
                                        newQ = D.foldr(\(x,y,c) acc -> case (exist y visited) of
                                                                            True -> acc
                                                                            False -> (y,c+distance):acc) newQueue matched
                                     in findPath source goal (visited ++ [minNode]) (Graph nodes rest) (sortQueue newQ) newAdjaceny



exist ::(Eq k) => k -> [k] -> Bool
exist node list = elem node list

firstValue :: (a,a,Integer) -> a
firstValue (a,_,_) = a


sortQueue :: [Node a] -> [Node a]
sortQueue list = sortBy func list
            where
                func (a,val) (b,value)
                    |val < value = LT
                    |val > value = GT
                    |val == value = EQ

delMin ::[Node a] -> (Node a,[Node a])
delMin ((x,val):xs) = ((x,val),sortQueue xs)



getAdjacency ::(Ord k) => Integer -> [k] -> [(a,k,Integer)] -> Map.Map k (Node a) -> Map.Map k (Node a)
getAdjacency distance visited matched adjacency = D.foldr (\(x,y,d) acc ->
    case (exist y visited) of
            True -> acc
            False -> case (Map.lookup y adjacency) of
                        Nothing -> Map.insert y (x,d) acc
                        Just (nod,value) ->
                                      case ((d + distance) < value) of
                                        True -> (Map.insert y (x,d+distance) acc)
                                        False -> (Map.insert y (nod,value) acc)) adjacency matched
