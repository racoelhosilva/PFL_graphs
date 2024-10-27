module TP1 where

import qualified Data.List
-- import qualified Data.Array
-- import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]

-- -- TODO: Can edges be self-loops?
-- toDirected :: RoadMap -> RoadMap
-- toDirected roadMap = roadMap ++ [(dest, orig, dist) | (orig, dest, dist) <- roadMap, orig /= dest]

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x1:x2:xs)
  | x1 == x2  = uniq (x2:xs)
  | otherwise = x1 : uniq (x2:xs)

sortUniq :: Ord a => [a] -> [a]
sortUniq xs = uniq $ Data.List.sort xs

cities :: RoadMap -> [City]
cities [] = []
cities r = sortUniq $ citySelect r
  where
    citySelect :: RoadMap -> [City]
    citySelect [] = []
    citySelect ((a, b, _):xs) = a : b : citySelect xs

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any connectsCities roadMap
  where
    connectsCities :: (City, City, Distance) -> Bool
    connectsCities (orig, dest, _) = (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = if null match then Nothing else Just (head match)
  where
    match = [dist | (orig, dest, dist) <- roadMap, (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)]

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(dest, dist) | (orig, dest, dist) <- roadMap, orig == city] ++ [(orig, dist) | (orig, dest, dist) <- roadMap, dest == city]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Nothing
pathDistance roadMap path = sum <$> sequence distances
  where
    consecutivePairs :: [(City, City)]
    consecutivePairs = zip path (tail path)

    distances :: [Maybe Distance]
    distances = map (uncurry $ distance roadMap) consecutivePairs

rome :: RoadMap -> [City]
rome roadMap = map fst (filter (\(c,d) -> d == maxDegree) degrees)
  where
    degrees :: [(City, Int)]
    degrees = [(city, length (adjacent roadMap city)) | city <- cities roadMap]
    maxDegree :: Int
    maxDegree = maximum (map snd degrees)

-- isStronglyConnected

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

reverseEdge :: (City,City,Distance) -> (City,City,Distance)
reverseEdge (orig, dest, dist) = (dest, orig, dist)

reverseGraph :: RoadMap -> RoadMap
reverseGraph roadMap = [reverseEdge edge | edge <- roadMap]

sortedAdjacent :: RoadMap -> City -> ([(City,Distance)],RoadMap)
sortedAdjacent [] _ = ([], []) 
sortedAdjacent ((orig, dest, dist):subRoadMap) city
  | orig == city = ((dest, dist) : subAdjacents, finalRoadMap)
  | otherwise    = ([], (orig, dest, dist):subRoadMap)
    where
      (subAdjacents, finalRoadMap) = sortedAdjacent subRoadMap city

sortedAdjacents :: RoadMap -> [City] -> [[(City,Distance)]]
sortedAdjacents _ [] = []
sortedAdjacents roadMap (city:cities) = adjacent : sortedAdjacents subRoadMap cities
  where
    (adjacent, subRoadMap) = sortedAdjacent roadMap city

toAdjList :: RoadMap -> AdjList
toAdjList roadMap = zipWith3 (\city adj1 adj2 -> (city, merge adj1 adj2)) mapCities adjs revAdjs
  where
    mapCities :: [City]
    mapCities = cities roadMap

    adjs :: [[(City,Distance)]]
    adjs =  sortedAdjacents (Data.List.sort roadMap) mapCities
    
    revAdjs :: [[(City,Distance)]]
    revAdjs = sortedAdjacents (Data.List.sort $ reverseGraph roadMap) mapCities


adjacent' :: AdjList -> City -> [(City,Distance)]
adjacent' adjList city = case lookup city adjList of
  (Just adj) -> adj
  Nothing    -> []

dfs :: AdjList -> City -> [Bool]
dfs adjList root = dfsVisit visitedList root
  where
    visitedList :: [Bool]
    visitedList = [False | _ <- adjList]

    setVisited :: AdjList -> City -> [Bool] -> [Bool]
    setVisited [] _ _ = []
    setVisited ((city,_):subAdjList) targetCity (visited:subVisitedList)
      | city == targetCity = True : subVisitedList
      | otherwise          = visited : setVisited subAdjList targetCity subVisitedList

    getUnvisitedAdjs :: AdjList -> [(City,Distance)] -> [Bool] -> [City]
    getUnvisitedAdjs _ [] _ = []
    getUnvisitedAdjs ((city,_):subAdjList) ((adj,dist):adjacents) (visited:subVisitedList)
      | city < adj  = getUnvisitedAdjs subAdjList ((adj,dist):adjacents) subVisitedList
      | not visited = city : getUnvisitedAdjs subAdjList adjacents subVisitedList
      | otherwise   = getUnvisitedAdjs subAdjList adjacents subVisitedList

    dfsVisit :: [Bool] -> City -> [Bool]
    dfsVisit visitedList root = foldl dfsVisit nextVisitedList unvisitedAdjs
      where
        nextVisitedList :: [Bool]
        nextVisitedList = setVisited adjList root visitedList

        adjs :: [(City,Distance)]
        adjs = adjacent' adjList root

        unvisitedAdjs :: [City]
        unvisitedAdjs = getUnvisitedAdjs adjList adjs visitedList


isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = and $ dfs adjList root
  where
    adjList :: AdjList
    adjList = toAdjList roadMap

    root :: City
    root = fst $ head adjList

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
