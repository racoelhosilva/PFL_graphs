module TP1 where

import qualified Data.Array
import qualified Data.Bits
import qualified Data.List

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

type AdjList = [(City, [(City, Distance)])]
type AdjMap = Map City [(City, Distance)]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)

-- Bitmask

type Bitmask = Int

emptyBitmask :: Bitmask
emptyBitmask = 0

fullBitmask :: Int -> Bitmask
fullBitmask n
  | n >= 0 && n < 30 = 1 `Data.Bits.shiftL` n - 1
  | n == 30          = minBound :: Int
  | otherwise        = error "Bitmask representation size out of bounds"

setBit :: Bitmask -> Int -> Bitmask
setBit = Data.Bits.setBit

clearBit :: Bitmask -> Int -> Bitmask
clearBit = Data.Bits.clearBit

toggleBit :: Bitmask -> Int -> Bitmask
toggleBit = Data.Bits.complementBit

isBitSet :: Bitmask -> Int -> Bool
isBitSet = Data.Bits.testBit

bitmaskToList :: Bitmask -> [Int]
bitmaskToList bitmask = toListAcc bitmask 0
  where
    toListAcc :: Bitmask -> Int -> [Int]
    toListAcc 0 _ = []
    toListAcc bitmask pos
      | odd bitmask = pos : toListAcc (bitmask `div` 2) (pos + 1)
      | otherwise   = toListAcc (bitmask `div` 2) (pos + 1)

-- Set implementation based on an AVL Tree

data Set a = SEmpty | SNode a (Set a) (Set a) Int
  deriving (Show, Eq)

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
rotateRight (SNode y (SNode x l lx h2) r h1) = updateHeight $ SNode x l (updateHeight (SNode y lx r undefined)) undefined
rotateRight s = s

rotateLeft :: Set a -> Set a
rotateLeft (SNode x l (SNode y rx r h2) h1) = updateHeight $ SNode y (updateHeight (SNode x l rx undefined)) r undefined
rotateLeft s = s

balance :: Set a -> Set a
balance (SNode val l r h)
  | bf > 1 && balanceFactor l >= 0 = rotateRight (SNode val l r h)
  | bf > 1 = rotateRight (SNode val (rotateLeft l) r h)
  | bf < -1 && balanceFactor r <= 0 = rotateLeft (SNode val l r h)
  | bf < -1 = rotateLeft (SNode val l (rotateRight r) h)
  | otherwise = updateHeight (SNode val l r h)
  where
    bf = balanceFactor (SNode val l r h)
balance s = s

insertSet :: (Ord a) => Set a -> a -> Set a
insertSet SEmpty newVal = SNode newVal SEmpty SEmpty 1
insertSet (SNode val l r h) newVal
  | val > newVal = balance (SNode val (insertSet l newVal) r h)
  | val < newVal = balance (SNode val l (insertSet r newVal) h)
  | otherwise = SNode newVal l r h

containsSet :: (Ord a) => Set a -> a -> Bool
containsSet SEmpty _ = False
containsSet (SNode val l r h) target
  | val > target = containsSet l target
  | val < target = containsSet r target
  | otherwise = True

searchSet :: (Ord a) => Set a -> a -> Set a
searchSet SEmpty _ = SEmpty
searchSet (SNode val l r h) target
  | val > target = searchSet l target
  | val < target = searchSet r target
  | otherwise = SNode val l r h

sizeSet :: Set a -> Int
sizeSet SEmpty = 0
sizeSet (SNode _ l r _) = 1 + sizeSet l + sizeSet r

setToList :: Set a -> [a]
setToList SEmpty = []
setToList (SNode v l r _) = setToList l ++ [v] ++ setToList r

-- Map

data MEntry k v = MEntry k v
  deriving (Show)

instance (Eq k) => Eq (MEntry k v) where
  (MEntry k1 _) == (MEntry k2 _) = k1 == k2

instance (Ord k) => Ord (MEntry k v) where
  (MEntry k1 _) `compare` (MEntry k2 _) = k1 `compare` k2

newtype (Ord k) => Map k v = Map (Set (MEntry k v))
  deriving (Show, Eq)

emptyMap :: (Ord k) => Map k v
emptyMap = Map emptySet

insertMap :: (Ord k) => Map k v -> k -> v -> Map k v
insertMap (Map s) key value = Map (insertSet s (MEntry key value))

lookupMap :: (Ord k) => Map k v -> k -> Maybe v
lookupMap (Map s) key =
  case searchSet s (MEntry key undefined) of
    SEmpty -> Nothing
    SNode (MEntry _ v) _ _ _ -> Just v

sizeMap :: Ord k => Map k v -> Int
sizeMap (Map s) = sizeSet s

mapFromList :: (Ord k) => [(k, v)] -> Map k v
mapFromList [] = emptyMap
mapFromList ((key, val) : subList) = insertMap (mapFromList subList) key val

mapToList :: (Ord k) => Map k v -> [(k, v)]
mapToList (Map SEmpty) = []
mapToList (Map (SNode (MEntry k v) l r _)) = mapToList (Map l) ++ [(k, v)] ++ mapToList (Map r)

-- Binary Heap

data (Ord a) => Heap a = HNode a Int (Heap a) (Heap a) | HEmpty
  deriving (Show, Eq)

emptyHeap :: Heap a
emptyHeap = HEmpty

heapSize :: (Ord a) => Heap a -> Int
heapSize HEmpty = 0
heapSize (HNode _ size _ _) = size

heapMin :: (Ord a) => Heap a -> a
heapMin (HNode min _ _ _) = min

addLast :: (Ord a) => Heap a -> a -> Heap a
addLast HEmpty x = HNode x 1 HEmpty HEmpty
addLast (HNode y size left right) x
  | heapSize left <= heapSize right = HNode y (size + 1) (addLast left x) right
  | otherwise = HNode y (size + 1) left (addLast right x)

heapifyUp :: (Ord a) => Heap a -> Heap a
heapifyUp HEmpty = HEmpty
heapifyUp (HNode x size left right)
  | heapSize left > heapSize right =
      let newLeft = heapifyUp left
       in case heapifyUp left of
            (HNode y size' left' right') ->
              if y < x
                then HNode y size (HNode x size' left' right') right
                else HNode x size newLeft right
            _ -> HNode x size newLeft right
  | otherwise =
      let newRight = heapifyUp right
       in case heapifyUp right of
            (HNode y size' left' right') ->
              if y < x
                then HNode y size left (HNode x size' left' right')
                else HNode x size left newRight
            _ -> HNode x size left newRight

heapInsert :: (Ord a) => Heap a -> a -> Heap a
heapInsert heap x = heapifyUp $ addLast heap x

removeLast :: (Ord a) => Heap a -> (a, Heap a)
removeLast (HNode x _ HEmpty HEmpty) = (x, HEmpty)
removeLast (HNode x size left right)
  | heapSize left > heapSize right = let (lastVal, newLeft) = removeLast left in (lastVal, HNode x (size - 1) newLeft right)
  | otherwise = let (lastVal, newRight) = removeLast right in (lastVal, HNode x (size - 1) left newRight)

heapifyDown :: (Ord a) => Heap a -> Heap a
heapifyDown (HNode x size left@(HNode y size' left' right') right@(HNode z size'' left'' right''))
  | y < x && y < z = HNode y size (heapifyDown $ HNode x size' left' right') right
  | z < x           = HNode z size left (heapifyDown $ HNode x size'' left'' right'')
heapifyDown (HNode x size (HNode y size' left' right') HEmpty)
  | y < x = HNode y size (heapifyDown $ HNode x size' left' right') HEmpty
heapifyDown (HNode x size HEmpty (HNode y size' left' right'))
  | y < x = HNode y size HEmpty (heapifyDown $ HNode x size' left' right')
heapifyDown heap = heap

heapPopMin :: (Ord a) => Heap a -> (a, Heap a)
heapPopMin heap =
  let (lastVal, newTree) = removeLast heap
  in case newTree of
    (HNode x size left right) -> (x, heapifyDown (HNode lastVal size left right))
    HEmpty                    -> (lastVal, HEmpty)

heapIsEmpty :: (Ord a) => Heap a -> Bool
heapIsEmpty HEmpty = True
heapIsEmpty _ = False

-- cities

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x1 : x2 : xs)
  | x1 == x2 = uniq (x2 : xs)
  | otherwise = x1 : uniq (x2 : xs)

sortUniq :: (Ord a) => [a] -> [a]
sortUniq xs = uniq $ Data.List.sort xs

cities :: RoadMap -> [City]
cities [] = []
cities r = sortUniq $ citySelect r
  where
    citySelect :: RoadMap -> [City]
    citySelect [] = []
    citySelect ((a, b, _) : xs) = a : b : citySelect xs

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

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadMap city = [(dest, dist) | (orig, dest, dist) <- roadMap, orig == city] ++ [(orig, dist) | (orig, dest, dist) <- roadMap, dest == city]

-- pathDistance

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

toAdjList :: RoadMap -> AdjList
toAdjList roadMap = zipWith3 (\city adj1 adj2 -> (city, merge adj1 adj2)) mapCities adjs revAdjs
  where
    mapCities :: [City]
    mapCities = cities roadMap

    adjs :: [[(City, Distance)]]
    adjs = sortedAdjacents (Data.List.sort roadMap) mapCities

    revAdjs :: [[(City, Distance)]]
    revAdjs = sortedAdjacents (Data.List.sort $ reverseGraph roadMap) mapCities

reverseEdge :: (City, City, Distance) -> (City, City, Distance)
reverseEdge (orig, dest, dist) = (dest, orig, dist)

reverseGraph :: RoadMap -> RoadMap
reverseGraph roadMap = [reverseEdge edge | edge <- roadMap]

sortedAdjacent :: RoadMap -> City -> ([(City, Distance)], RoadMap)
sortedAdjacent [] _ = ([], [])
sortedAdjacent ((orig, dest, dist) : subRoadMap) city
  | orig == city = ((dest, dist) : subAdjacents, finalRoadMap)
  | otherwise = ([], (orig, dest, dist) : subRoadMap)
  where
    (subAdjacents, finalRoadMap) = sortedAdjacent subRoadMap city

sortedAdjacents :: RoadMap -> [City] -> [[(City, Distance)]]
sortedAdjacents _ [] = []
sortedAdjacents roadMap (city : cities) = adjacent : sortedAdjacents subRoadMap cities
  where
    (adjacent, subRoadMap) = sortedAdjacent roadMap city

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

toAdjMap :: RoadMap -> AdjMap
toAdjMap roadMap = foldl insertEdge emptyMap (roadMap ++ reverseGraph roadMap)
  where
    insertEdge :: AdjMap -> (City, City, Distance) -> AdjMap
    insertEdge map (orig, dest, dist) = insertMap map orig ((dest, dist) : oldAdj)
      where
        oldAdj :: [(City, Distance)]
        oldAdj = case lookupMap map orig of
          Just adj -> adj
          Nothing -> []

unjust :: Maybe a -> a
unjust (Just x) = x
unjust Nothing = undefined

dfs :: AdjMap -> City -> Set City
dfs adjMap root = dfsVisit visitedSet root
  where
    visitedSet :: Set City
    visitedSet = emptySet

    dfsVisit :: Set City -> City -> Set City
    dfsVisit visitedSet root = getNextVisitedSet (insertSet visitedSet root) adjs
      where
        adjs :: [City]
        adjs = map fst (unjust $ lookupMap adjMap root)

        getNextVisitedSet :: Set City -> [City] -> Set City
        getNextVisitedSet visitedSet [] = visitedSet
        getNextVisitedSet visitedSet (adj : adjs)
          | containsSet visitedSet adj = getNextVisitedSet visitedSet adjs
          | otherwise = getNextVisitedSet (dfsVisit visitedSet adj) adjs

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = and $ [containsSet visitedSet city | city <- cities roadMap]
  where
    adjMap :: AdjMap
    adjMap = toAdjMap roadMap

    root :: City
    root = fst $ head roadMap
      where
        fst :: (a, b, c) -> a
        fst (a, _, _) = a

    visitedSet :: Set City
    visitedSet = dfs adjMap root

-- shortestPath

type DijkstraState = (Distance, City, [Path])
type Predecessors = (Distance, [Path])

dijkstra :: AdjMap -> Map City Predecessors -> Heap DijkstraState -> Map City Predecessors
dijkstra adjMap predMap heap
  | heapIsEmpty heap = predMap
  | otherwise        = dijkstra adjMap newMap newHeap

  where
    restHeap :: Heap DijkstraState
    ((newDist, city, paths), restHeap) = heapPopMin heap

    (newMap, newHeap) = case lookupMap predMap city of
      Just (oldDist,oldPaths) -> if newDist == oldDist
        then (insertMap predMap city (newDist, paths ++ oldPaths), restHeap)
        else (predMap, restHeap)
      Nothing                 -> (insertMap predMap city (newDist, paths), updatedHeap)

    updatedHeap :: Heap DijkstraState
    updatedHeap = foldl insertState restHeap adjs
      where
        adjs :: [(City,Distance)]
        adjs = unjust $ lookupMap adjMap city

        insertState :: Heap DijkstraState -> (City, Distance) -> Heap DijkstraState
        insertState heap (neighbor, dist) = heapInsert heap (newDist + dist, neighbor, map (neighbor :) paths)

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap orig dest = case lookupMap pathMap dest of
  (Just (_,paths)) -> map reverse paths
  Nothing          -> []
  where
    adjMap :: AdjMap
    adjMap = toAdjMap roadMap

    pathMap = dijkstra adjMap emptyMap (heapInsert emptyHeap (0, orig, [[orig]]))

-- travelSales

mapToIndexes :: Ord a => [a] -> Map a Int
mapToIndexes xs = mapFromList $ zip xs [0..]

toAdjMatrix :: RoadMap -> Map City Int -> AdjMatrix
toAdjMatrix roadMap indexMap = Data.Array.accumArray (\_ d -> Just d) Nothing ((0, 0), (numCities - 1, numCities - 1)) edges
  where
    numCities :: Int
    numCities = sizeMap indexMap

    getIndex :: City -> Int
    getIndex city = unjust $ lookupMap indexMap city

    edges :: [((Int,Int),Distance)]
    edges = [((getIndex c1, getIndex c2), d) | (c1, c2, d) <- roadMap ++ reverseGraph roadMap]

type TspEntry = (Maybe Distance,[Int])

heldKarp :: AdjMatrix -> TspEntry
heldKarp adjMatrix = dp Data.Array.! (start, fullBitmask (numCities - 1))
  where
    numCities :: Int
    numCities = snd (snd $ Data.Array.bounds adjMatrix) + 1

    start :: Int
    start = numCities - 1

    dp :: Data.Array.Array (Int,Bitmask) TspEntry
    dp = Data.Array.listArray bounds [getTspEntry city bitmask | city <- [0..numCities - 1], bitmask <- [0..fullBitmask numCities]]
      where
        bounds :: ((Int,Bitmask),(Int,Bitmask))
        bounds = ((0,emptyBitmask),(numCities - 1,fullBitmask numCities))

        compEntry :: TspEntry -> TspEntry -> Ordering
        compEntry (Just dist1,_) (Just dist2,_) = dist1 `compare` dist2
        compEntry (Just _,_) (Nothing,_) = LT
        compEntry (Nothing,_) (Just _,_) = GT
        compEntry (Nothing,_) (Nothing,_) = EQ

        addCity :: TspEntry -> Int -> TspEntry
        addCity (Nothing, path) city  = (Nothing, city : path)
        addCity (Just totalDist, pred:pathTail) city = case adjMatrix Data.Array.! (city, pred) of
          Just dist -> (Just (totalDist + dist), city : pred: pathTail)
          Nothing   -> (Nothing, city : pred: pathTail)

        getTspEntry :: Int -> Bitmask -> TspEntry
        getTspEntry city bitmask
          | bitmask == emptyBitmask = (adjMatrix Data.Array.! (city, start), [city, start])
          | otherwise               = Data.List.minimumBy compEntry
            [addCity (dp Data.Array.! (other, clearBit bitmask other)) city | other <- bitmaskToList bitmask]

travelSales :: RoadMap -> Path
travelSales [] = []
travelSales roadMap = case heldKarp adjMatrix of
  (Just _, path) -> map getCity path
  (Nothing, _)   -> []
  where
    indexMap :: Map City Int
    indexMap = mapToIndexes $ cities roadMap

    adjMatrix :: AdjMatrix
    adjMatrix = toAdjMatrix roadMap indexMap

    cityMap :: Map Int City
    cityMap = mapFromList $ map (\(x,y) -> (y,x)) (mapToList indexMap)

    getCity :: Int -> City
    getCity index = unjust $ lookupMap cityMap index

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]
