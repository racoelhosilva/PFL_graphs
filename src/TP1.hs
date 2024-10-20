module TP1 where

import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

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
areAdjacent roadMap city1 city2 = any connectsCities roadMap where
  connectsCities :: (City, City, Distance) -> Bool
  connectsCities (orig, dest, _) = (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = if null match then Nothing else Just (head match)
  where 
    match = [dist | (orig, dest, dist) <- roadMap, (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)]

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(dest, dist) | (orig, dest, dist) <- roadMap, orig == city] ++ [(orig, dist) | (orig, dest, dist) <- roadMap, dest == city]

pathDistance :: RoadMap -> Path -> Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

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
