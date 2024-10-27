module TP1 where

import qualified Data.List
-- import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]

-- Bitmask

newtype Bitmask = Bitmask Integer

newBitmask :: Bitmask
newBitmask = Bitmask 0

setBit :: Bitmask -> Int -> Bitmask
setBit (Bitmask bits) pos = Bitmask (Data.Bits.setBit bits pos)

clearBit :: Bitmask -> Int -> Bitmask
clearBit (Bitmask bits) pos = Bitmask (Data.Bits.clearBit bits pos)

toggleBit :: Bitmask -> Int -> Bitmask
toggleBit (Bitmask bits) pos = Bitmask (Data.Bits.complementBit bits pos)

isBitSet :: Bitmask -> Int -> Bool
isBitSet (Bitmask bits) = Data.Bits.testBit bits

-- Set implementation based on an AVL Tree

data Set a = SEmpty | SNode a (Set a) (Set a) Int
  deriving (Show, Eq, Ord)

emptySet :: Set a
emptySet = SEmpty

height :: Set a -> Int
height SEmpty = 0
height (SNode _ _ _ h) = h

updateHeight :: Set a -> Set a
updateHeight SEmpty = SEmpty
updateHeight (SNode v l r _) = SNode v l r (1 + max (height l) (height r))

balanceFactor :: Set a -> Int
balanceFactor SEmpty = 0
balanceFactor (SNode _ l r _) = height l - height r

rotateRight :: Set a -> Set a
rotateRight (SNode y (SNode x l lx h2) r h1) = updateHeight $ SNode x l (updateHeight (SNode y lx r h2)) h1
rotateRight s = s

rotateLeft :: Set a -> Set a
rotateLeft (SNode x l (SNode y rx r h2) h1) = updateHeight $ SNode y (updateHeight (SNode x l rx h2)) r h1
rotateLeft s = s

balance :: Set a -> Set a
balance (SNode val l r h)
  | bf > 1 && balanceFactor l >= 0  = rotateRight (SNode val l r h)
  | bf > 1                          = rotateRight (SNode val (rotateLeft l) r h)
  | bf < -1 && balanceFactor r <= 0 = rotateLeft (SNode val l r h)
  | bf < -1                         = rotateLeft (SNode val l (rotateRight r) h)
  | otherwise                       = updateHeight (SNode val l r h)
  where bf = balanceFactor (SNode val l r h)
balance s = s

insertSet :: (Ord a) => Set a -> a -> Set a
insertSet SEmpty newVal = SNode newVal SEmpty SEmpty 1
insertSet (SNode val l r h) newVal
  | val > newVal  = balance (SNode val (insertSet l newVal) r h)
  | val < newVal  = balance (SNode val l (insertSet r newVal) h)
  | otherwise     = SNode val l r h

containsSet :: (Ord a) => Set a -> a -> Bool
containsSet SEmpty _ = False
containsSet (SNode val l r h) target
  | val > target  = containsSet l target
  | val < target  = containsSet r target
  | otherwise   = True

searchSet :: (Ord a) => Set a -> a -> Set a
searchSet SEmpty _ = SEmpty
searchSet (SNode val l r h) target
  | val > target  = searchSet l target
  | val < target  = searchSet r target
  | otherwise     = SNode val l r h 

setToList :: Set a -> [a]
setToList SEmpty = []
setToList (SNode v l r _) = setToList l ++ [v] ++ setToList r

-- Binary Heap

data Ord a => Heap a = HNode a Int (Heap a) (Heap a) | HEmpty

emptyHeap :: Heap a
emptyHeap = HEmpty

heapSize :: Ord a => Heap a -> Int
heapSize HEmpty = 0
heapSize (HNode _ size _ _) = size

heapMin :: Ord a => Heap a -> a
heapMin (HNode min _ _ _) = min

addLast :: Ord a => Heap a -> a -> Heap a
addLast HEmpty x = HNode x 1 HEmpty HEmpty
addLast (HNode y size left right) x
  | heapSize left <= heapSize right = HNode y (size + 1) (addLast left x) right
  | otherwise                       = HNode y (size + 1) left (addLast right x)

heapifyUp :: Ord a => Heap a -> Heap a
heapifyUp HEmpty = HEmpty
heapifyUp (HNode x size left right)
  | heapSize left > heapSize right = let newLeft = heapifyUp left
    in case heapifyUp left of
      (HNode y size' left' right') -> if y < x
        then HNode y size (HNode x size' left' right') right
        else HNode x size newLeft right
      _ -> HNode x size newLeft right
  | otherwise = let newRight = heapifyUp right
    in case heapifyUp right of
      (HNode y size' left' right') -> if y < x
        then HNode y size left (HNode x size' left' right')
        else HNode x size left newRight
      _ -> HNode x size left newRight

heapInsert :: Ord a => Heap a -> a -> Heap a
heapInsert heap x = heapifyUp $ addLast heap x

removeLast :: Ord a => Heap a -> (a, Heap a)
removeLast (HNode x _ HEmpty HEmpty) = (x, HEmpty)
removeLast (HNode x size left right)
  | heapSize left > heapSize right = let (lastVal, newLeft) = removeLast left in (lastVal, HNode x (size - 1) newLeft right)
  | otherwise                      = let (lastVal, newRight) = removeLast right in (lastVal, HNode x (size - 1) left newRight)

heapifyDown :: Ord a => Heap a -> Heap a
heapifyDown (HNode x size left@(HNode y size' left' right') right@(HNode z size'' left'' right''))
  | y < x && y < z = HNode y size (HNode x size' left' right') right
  | z < x          = HNode z size left (HNode x size'' left'' right'')
heapifyDown (HNode x size (HNode y size' left' right') HEmpty)
  | y < x = HNode y size (HNode x size' left' right') HEmpty
heapifyDown (HNode x size HEmpty (HNode y size' left' right'))
  | y < x = HNode y size (HNode x size' left' right') HEmpty
heapifyDown heap = heap

heapPopMin :: Ord a => Heap a -> (a, Heap a)
heapPopMin heap = let (lastVal, newTree) = removeLast heap
  in case newTree of
    (HNode x size left right) -> (x, heapifyDown (HNode lastVal size left right))
    HEmpty -> (lastVal, HEmpty)

heapIsEmpty :: Ord a => Heap a -> Bool
heapIsEmpty HEmpty = True
heapIsEmpty _ = False

-- cities

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

-- areAdjacent

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any connectsCities roadMap
  where
    connectsCities :: (City, City, Distance) -> Bool
    connectsCities (orig, dest, _) = (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)

-- distance

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = if null match then Nothing else Just (head match)
  where
    match = [dist | (orig, dest, dist) <- roadMap, (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)]

-- adjacent

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(dest, dist) | (orig, dest, dist) <- roadMap, orig == city] ++ [(orig, dist) | (orig, dest, dist) <- roadMap, dest == city]

-- pathDistance

-- TODO: Optimize
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Nothing
pathDistance roadMap path = sum <$> sequence distances
  where
    consecutivePairs :: [(City, City)]
    consecutivePairs = zip path (tail path)

    distances :: [Maybe Distance]
    distances = map (uncurry $ distance roadMap) consecutivePairs

-- rome

rome :: RoadMap -> [City]
rome roadMap = map fst (filter (\(city, degree) -> degree == maxDegree) degrees)
  where 
    adjList :: AdjList
    adjList = toAdjList roadMap

    degrees :: [(City, Int)]
    degrees = [(city, length adj) | (city, adj) <- adjList]
    
    maxDegree :: Int
    maxDegree = maximum $ map snd degrees

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

unjust :: Maybe a -> a
unjust (Just x) = x
unjust Nothing = undefined

adjacent' :: AdjList -> City -> [(City,Distance)]
adjacent' adjList city = unjust $ lookup city adjList

getCityIndex :: AdjList -> City -> Int
getCityIndex roadMap city = unjust $ Data.List.elemIndex city $ map fst roadMap

dfs :: AdjList -> City -> Set City
dfs adjList root = dfsVisit visitedSet root
  where
    visitedSet :: Set City
    visitedSet = emptySet

    dfsVisit :: Set City -> City -> Set City
    dfsVisit visitedSet root = foldl dfsVisit nextVisitedSet unvisitedAdjs
      where
        nextVisitedSet :: Set City
        nextVisitedSet = insertSet visitedSet root

        adjs :: [(City,Distance)]
        adjs = adjacent' adjList root

        unvisitedAdjs :: [City]
        unvisitedAdjs = [city | (city,_) <- adjs, not (containsSet visitedSet city)]


isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = and $ [containsSet res city | city <- cities roadMap]
  where
    adjList :: AdjList
    adjList = toAdjList roadMap

    root :: City
    root = fst $ head adjList

    res :: Set City
    res = dfs adjList root

-- shortestPath

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

-- travelSales

travelSales :: RoadMap -> Path
travelSales = undefined

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
