{- PFL 2024/2025 Practical assignment 1 -}
module TP1 where

{- Modules Imported -}

import qualified Data.Array
import qualified Data.Bits
import qualified Data.List

{- Type Definitions -}

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

type DijkstraState = (Distance, City, [Path])

type Predecessors = (Distance, [Path])

type TspEntry = (Maybe Distance, [Int])

{- Graph Representations -}

type AdjList = [(City, [(City, Distance)])]

type AdjMap = Map City [(City, Distance)]

type AdjMatrix = Data.Array.Array (Int, Int) (Maybe Distance)

-- | Convert a roadmap into an adjacency list representation.
--
--   Efficiency:
--     * Time Complexity: O(E log E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--
--   Returns:
--     * AdjMatrix: representation of the graph.
toAdjList :: RoadMap -> AdjList
toAdjList roadMap = zipWith3 (\city adj1 adj2 -> (city, merge adj1 adj2)) mapCities adjs revAdjs
  where
    mapCities :: [City]
    mapCities = cities roadMap

    adjs :: [[(City, Distance)]]
    adjs = sortedAdjacents (Data.List.sort roadMap) mapCities

    revAdjs :: [[(City, Distance)]]
    revAdjs = sortedAdjacents (Data.List.sort $ reverseGraph roadMap) mapCities

-- | Convert a roadmap into an adjacency map representation.
--
--   Efficiency:
--     * Time Complexity: O(E log E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--
--   Returns:
--     * AdjMatrix: representation of the graph.
toAdjMap :: RoadMap -> AdjMap
toAdjMap roadMap = foldl insertEdge emptyMap (roadMap ++ reverseGraph roadMap)
  where
    insertEdge :: AdjMap -> (City, City, Distance) -> AdjMap
    insertEdge map (orig, dest, dist) = mapInsert map orig ((dest, dist) : oldAdj)
      where
        oldAdj :: [(City, Distance)]
        oldAdj = case mapLookup map orig of
          Just adj -> adj
          Nothing  -> []

-- | Convert a roadmap into an adjacency matrix representation.
--
--   Efficiency:
--     * Time Complexity: O(V²)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--     * Map City Int: mapping conversions of the vertices to numbers.
--
--   Returns:
--     * AdjMatrix: representation of the graph.
toAdjMatrix :: RoadMap -> Map City Int -> AdjMatrix
toAdjMatrix roadMap indexMap = Data.Array.accumArray (\_ d -> Just d) Nothing ((0, 0), (numCities - 1, numCities - 1)) edges
  where
    numCities :: Int
    numCities = mapSize indexMap

    getIndex :: City -> Int
    getIndex city = unjust $ mapLookup indexMap city

    edges :: [((Int, Int), Distance)]
    edges = [((getIndex c1, getIndex c2), d) | (c1, c2, d) <- roadMap ++ reverseGraph roadMap]

{- Data Structures: Bitmask -}

type Bitmask = Int

emptyBitmask :: Bitmask
emptyBitmask = 0

fullBitmask :: Int -> Bitmask
fullBitmask n
  | n >= 0 && n < 30 = 1 `Data.Bits.shiftL` n - 1
  | n == 30 = minBound :: Int
  | otherwise = error "Bitmask representation size out of bounds"

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
      | otherwise = toListAcc (bitmask `div` 2) (pos + 1)

{- Data Structures: Set (AVL Tree) -}

data Set a = SEmpty | SNode a (Set a) (Set a) Int
  deriving (Show, Eq)

emptySet :: Set a
emptySet = SEmpty

setHeight :: Set a -> Int
setHeight SEmpty          = 0
setHeight (SNode _ _ _ h) = h

setUpdateHeight :: Set a -> Set a
setUpdateHeight SEmpty = SEmpty
setUpdateHeight (SNode v l r _) = SNode v l r (1 + max (setHeight l) (setHeight r))

setBalanceFactor :: Set a -> Int
setBalanceFactor SEmpty          = 0
setBalanceFactor (SNode _ l r _) = setHeight l - setHeight r

setRotateRight :: Set a -> Set a
setRotateRight (SNode y (SNode x l lx h2) r h1) = setUpdateHeight $ SNode x l (setUpdateHeight (SNode y lx r undefined)) undefined
setRotateRight s = s

setRotateLeft :: Set a -> Set a
setRotateLeft (SNode x l (SNode y rx r h2) h1) = setUpdateHeight $ SNode y (setUpdateHeight (SNode x l rx undefined)) r undefined
setRotateLeft s = s

setBalance :: Set a -> Set a
setBalance (SNode val l r h)
  | bf > 1 && setBalanceFactor l >= 0 = setRotateRight (SNode val l r h)
  | bf > 1 = setRotateRight (SNode val (setRotateLeft l) r h)
  | bf < -1 && setBalanceFactor r <= 0 = setRotateLeft (SNode val l r h)
  | bf < -1 = setRotateLeft (SNode val l (setRotateRight r) h)
  | otherwise = setUpdateHeight (SNode val l r h)
  where
    bf = setBalanceFactor (SNode val l r h)
setBalance s = s

setInsert :: (Ord a) => Set a -> a -> Set a
setInsert SEmpty newVal = SNode newVal SEmpty SEmpty 1
setInsert (SNode val l r h) newVal
  | val > newVal = setBalance (SNode val (setInsert l newVal) r h)
  | val < newVal = setBalance (SNode val l (setInsert r newVal) h)
  | otherwise = SNode newVal l r h

setContains :: (Ord a) => Set a -> a -> Bool
setContains SEmpty _ = False
setContains (SNode val l r h) target
  | val > target = setContains l target
  | val < target = setContains r target
  | otherwise = True

searchSet :: (Ord a) => Set a -> a -> Set a
searchSet SEmpty _ = SEmpty
searchSet (SNode val l r h) target
  | val > target = searchSet l target
  | val < target = searchSet r target
  | otherwise = SNode val l r h

setSize :: Set a -> Int
setSize SEmpty          = 0
setSize (SNode _ l r _) = 1 + setSize l + setSize r

setToList :: Set a -> [a]
setToList SEmpty          = []
setToList (SNode v l r _) = setToList l ++ [v] ++ setToList r

{- Data Structures: Map -}

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

mapInsert :: (Ord k) => Map k v -> k -> v -> Map k v
mapInsert (Map s) key value = Map (setInsert s (MEntry key value))

mapLookup :: (Ord k) => Map k v -> k -> Maybe v
mapLookup (Map s) key =
  case searchSet s (MEntry key undefined) of
    SEmpty                   -> Nothing
    SNode (MEntry _ v) _ _ _ -> Just v

mapSize :: (Ord k) => Map k v -> Int
mapSize (Map s) = setSize s

mapFromList :: (Ord k) => [(k, v)] -> Map k v
mapFromList []                     = emptyMap
mapFromList ((key, val) : subList) = mapInsert (mapFromList subList) key val

mapToList :: (Ord k) => Map k v -> [(k, v)]
mapToList (Map SEmpty) = []
mapToList (Map (SNode (MEntry k v) l r _)) = mapToList (Map l) ++ [(k, v)] ++ mapToList (Map r)

{- Data Structures: Binary (Min) Heap -}

data (Ord a) => Heap a = HNode a Int (Heap a) (Heap a) | HEmpty
  deriving (Show, Eq)

emptyHeap :: Heap a
emptyHeap = HEmpty

heapRank :: (Ord a) => Heap a -> Int
heapRank HEmpty          = 0
heapRank (HNode _ r _ _) = r

heapMin :: (Ord a) => Heap a -> a
heapMin HEmpty            = error "Cannot pop empty heap"
heapMin (HNode min _ _ _) = min

heapBuild :: (Ord a) => a -> Heap a -> Heap a -> Heap a
heapBuild x h1 h2
  | heapRank h1 >= heapRank h2 = HNode x (heapRank h2 + 1) h1 h2
  | otherwise = HNode x (heapRank h1 + 1) h2 h1 

heapMerge :: (Ord a) => Heap a -> Heap a -> Heap a
heapMerge h HEmpty = h
heapMerge HEmpty h = h
heapMerge h1@(HNode x _ l1 r1) h2@(HNode y _ l2 r2)
  | x <= y = heapBuild x l1 (heapMerge r1 h2)
  | otherwise = heapBuild y l2 (heapMerge h1 r2)
  
heapInsert :: (Ord a) => Heap a -> a -> Heap a
heapInsert heap x = heapMerge (HNode x 1 HEmpty HEmpty) heap

heapPopMin :: (Ord a) => Heap a -> (a, Heap a)
heapPopMin HEmpty = error "Cannot pop empty heap"
heapPopMin (HNode x _ r l) = (x, heapMerge r l)

heapIsEmpty :: (Ord a) => Heap a -> Bool
heapIsEmpty HEmpty = True
heapIsEmpty _      = False

{- Auxiliary Functions -}

mapToIndexes :: (Ord a) => [a] -> Map a Int
mapToIndexes xs = mapFromList $ zip xs [0 ..]

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

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

sortUnique :: (Ord a) => [a] -> [a]
sortUnique xs = unique $ Data.List.sort xs

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x1 : x2 : xs)
  | x1 == x2 = unique (x2 : xs)
  | otherwise = x1 : unique (x2 : xs)

unjust :: Maybe a -> a
unjust (Just x) = x
unjust Nothing  = undefined

{- Function Definitions -}

-- | Returns all the cities of the roadmap.
--
--   Efficiency:
--     * Time Complexity: O(E log E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--
--   Returns:
--     * [City]: List of the cities (vertices) in the roadmap (graph).
cities :: RoadMap -> [City]
cities [] = []
cities r = sortUnique $ citySelect r
  where
    -- | Retrieves a list that, for each edge, contains both the start and end cities.
    --   Arguments:
    --     * Roadmap: representation of the graph.
    --   Returns:
    --     * [City]: All cities taken from the start or end of the edges in the roadmap.
    citySelect :: RoadMap -> [City]
    citySelect []               = []
    citySelect ((a, b, _) : xs) = a : b : citySelect xs

-- | Checks if two cities are adjacent in a roadmap.
--
--   Efficiency:
--     * Time Complexity: O(E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--     * City: vertex number 1.
--     * City: vertex number 2.
--
--   Returns:
--     * Bool: True if there is an edge that connects the two cities, False otherwise.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any connectsCities roadMap
  where
    -- | Checks if there an edge connects the two given cities (in either direction).
    --   Arguments:
    --     * (City, City, Distance): edge of the roadmap.
    --   Returns:
    --     * Bool: True if the edge connects them, False otherwise.
    connectsCities :: (City, City, Distance) -> Bool
    connectsCities (orig, dest, _) = (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)

-- | Returns the distance between two cities in a roadmap, if they are adjacent.
--
--   Efficiency:
--     * Time Complexity: O(E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--     * City: vertex number 1.
--     * City: vertex number 2.
--
--   Returns:
--     * Maybe Distance: Just distance, if there is an edge between them, Nothing otherwise.
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap city1 city2 = if null match then Nothing else Just (head match)
  where
    -- | List of the distances of edges between the given cities (presumably, between length is either 0 or 1)
    match :: [Distance]
    match = [dist | (orig, dest, dist) <- roadMap, (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)]

-- | Returns all of the cities adjacent to a city in a roadmap, as well as their distances.
--
--   Efficiency:
--     * Time Complexity: O(E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--     * City: central vertex.
--
--   Returns:
--     * [(City, Distance)]: List of pairs with the adjacent cities and their respective distances.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadMap city = [(dest, dist) | (orig, dest, dist) <- roadMap, orig == city] ++ [(orig, dist) | (orig, dest, dist) <- roadMap, dest == city]

-- | Returns the distance between a certain path in the roadmap.
--
--   Efficiency:
--     * Time Complexity: O(E log E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--     * Path: sequence of cities to consider.
--
--   Returns:
--     * Maybe Distance: Just the sum of the edge distance in the path, Nothing if any two consecutive cities are not connected.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Nothing
pathDistance roadMap path = getPathDistance sortedPairs sortedEdges
  where
    -- | All pairs of two consecutive cities in the path, sorted.
    sortedPairs :: [(City, City)]
    sortedPairs = Data.List.sort (zip path $ tail path)

    -- | All edges and reverse edges of the roadmap, sorted.
    sortedEdges :: [(City, City, Distance)]
    sortedEdges = Data.List.sort $ roadMap ++ reverseGraph roadMap

    -- | Computes the distance of a path with ordered pairs and edges.
    --   Arguments:
    --     * [(City, City)]: Pairs of cities in the path, sorted.
    --     * [(City, City, Distance)]: All edges of the graph, sorted.
    --   Returns:
    --     * Maybe Distance: Just sum of distances if all edges in the path exist, Nothing otherwise.
    getPathDistance :: [(City, City)] -> [(City, City, Distance)] -> Maybe Distance
    getPathDistance [] _ = Just 0
    getPathDistance _ [] = Nothing
    getPathDistance pairs@((orig1, dest1) : subPairs) edges@((orig2, dest2, dist) : subEdges)
      | (orig1, dest1) /= (orig2, dest2) = getPathDistance pairs subEdges
      | otherwise = case getPathDistance subPairs edges of
          Just restDist -> Just (restDist + dist)
          Nothing       -> Nothing

-- | Returns the cities in a roadmap with the highest adjacent cities (higher degree).
--
--   Efficiency:
--     * Time Complexity: O(E log E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--
--   Returns:
--     * [City]: Cities with the highest degree in the roadmap.
rome :: RoadMap -> [City]
rome roadMap = map fst (filter (\(city, degree) -> degree == maxDegree) degrees)
  where
    -- | Adjacency list representation of the roadmap.
    adjList :: AdjList
    adjList = toAdjList roadMap

    -- | Pair containing each city andd its degree
    degrees :: [(City, Int)]
    degrees = [(city, length adj) | (city, adj) <- adjList]

    -- | Highest degree across all cities i.
    maxDegree = maximum $ map snd degrees

-- | Returns whether all the cities in the roadmap are connected.
--
--   Efficiency:
--     * Time Complexity: O(E log E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--
--   Returns:
--     * Bool: True if the graph is strongly connected, False otherwise.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = and $ [setContains visitedSet city | city <- cities roadMap]
  where
    -- | Adjacency list representation of the roadmap.
    adjMap :: AdjMap
    adjMap = toAdjMap roadMap
    
    -- | Starting city for the DFS search.
    root :: City
    root = orig $ head roadMap
      where
        -- | Unpacks the origin city in the edge of the roadmap.
        --   Arguments:
        --     * (City, City, Distance): Edge of the roadmap.
        --   Returns:
        --     * City: First value in the edge triplet (origin city).
        orig :: (City, City, Distance) -> City
        orig (city, _, _) = city

    -- | Set of all visited cities after the DFS.
    visitedSet :: Set City
    visitedSet = dfs root

    -- | Returns whether all the cities in the roadmap are connected.
    --
    --   Efficiency:
    --     * Time Complexity: O(E log E)
    --
    --   Arguments:
    --     * City: starting city of the Depth First Search.
    --
    --   Returns:
    --     * Set City: set of visited cities by the end of the DFS.
    dfs :: City -> Set City
    dfs root = dfsVisit visitedSet root
      where
        -- | Set of all visited cities after the DFS.
        visitedSet :: Set City
        visitedSet = emptySet

        dfsVisit :: Set City -> City -> Set City
        dfsVisit visitedSet root = getNextVisitedSet (setInsert visitedSet root) adjs
          where
            adjs :: [City]
            adjs = map fst (unjust $ mapLookup adjMap root)

            getNextVisitedSet :: Set City -> [City] -> Set City
            getNextVisitedSet visitedSet [] = visitedSet
            getNextVisitedSet visitedSet (adj : adjs)
              | setContains visitedSet adj = getNextVisitedSet visitedSet adjs
              | otherwise = getNextVisitedSet (dfsVisit visitedSet adj) adjs

-- | Returns all the shortest paths that connect two cities in a roadmap.
--
--   Efficiency:
--     * Time Complexity: O(E log E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--     * City: starting point of the paths.
--     * City: end point of the paths.
--
--   Returns:
--     * [Path]: All single pair shortest paths of the given cities in a specific roadmap.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap orig dest = case mapLookup pathMap dest of
  (Just (_, paths)) -> map reverse paths
  Nothing           -> []
  where
    adjMap :: AdjMap
    adjMap = toAdjMap roadMap

    pathMap = dijkstra adjMap emptyMap (heapInsert emptyHeap (0, orig, [[orig]]))

    dijkstra :: AdjMap -> Map City Predecessors -> Heap DijkstraState -> Map City Predecessors
    dijkstra adjMap predMap heap
      | heapIsEmpty heap = predMap
      | otherwise = dijkstra adjMap newMap newHeap
      where
        restHeap :: Heap DijkstraState
        ((newDist, city, paths), restHeap) = heapPopMin heap

        (newMap, newHeap) = case mapLookup predMap city of
          Just (oldDist, oldPaths) ->
            if newDist == oldDist
              then (mapInsert predMap city (newDist, paths ++ oldPaths), restHeap)
              else (predMap, restHeap)
          Nothing -> (mapInsert predMap city (newDist, paths), updatedHeap)

        updatedHeap :: Heap DijkstraState
        updatedHeap = foldl insertState restHeap adjs
          where
            adjs :: [(City, Distance)]
            adjs = unjust $ mapLookup adjMap city

            insertState :: Heap DijkstraState -> (City, Distance) -> Heap DijkstraState
            insertState heap (neighbor, dist) = heapInsert heap (newDist + dist, neighbor, map (neighbor :) paths)

-- | Returns a solution for the Traveling Salesperson Problem (the shortest hamiltonian cycle in the graph).
--
--   Efficiency:
--     * Time Complexity: O(V² 2^V)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--
--   Returns:
--     * Path: Shortest hamiltonian cycle in the graph (shortest path that goes through all vertices and returns to the start).
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
    cityMap = mapFromList $ map (\(x, y) -> (y, x)) (mapToList indexMap)

    getCity :: Int -> City
    getCity index = unjust $ mapLookup cityMap index

    heldKarp :: AdjMatrix -> TspEntry
    heldKarp adjMatrix = dp Data.Array.! (start, fullBitmask (numCities - 1))
      where
        numCities :: Int
        numCities = snd (snd $ Data.Array.bounds adjMatrix) + 1

        start :: Int
        start = numCities - 1

        dp :: Data.Array.Array (Int, Bitmask) TspEntry
        dp = Data.Array.listArray bounds [getTspEntry city bitmask | city <- [0 .. numCities - 1], bitmask <- [0 .. fullBitmask numCities]]
          where
            bounds :: ((Int, Bitmask), (Int, Bitmask))
            bounds = ((0, emptyBitmask), (numCities - 1, fullBitmask numCities))

            compEntry :: TspEntry -> TspEntry -> Ordering
            compEntry (Just dist1, _) (Just dist2, _) = dist1 `compare` dist2
            compEntry (Just _, _) (Nothing, _)        = LT
            compEntry (Nothing, _) (Just _, _)        = GT
            compEntry (Nothing, _) (Nothing, _)       = EQ

            addCity :: TspEntry -> Int -> TspEntry
            addCity (Nothing, path) city = (Nothing, city : path)
            addCity (Just totalDist, pred : pathTail) city = case adjMatrix Data.Array.! (city, pred) of
              Just dist -> (Just (totalDist + dist), city : pred : pathTail)
              Nothing   -> (Nothing, city : pred : pathTail)

            getTspEntry :: Int -> Bitmask -> TspEntry
            getTspEntry city bitmask
              | bitmask == emptyBitmask = (adjMatrix Data.Array.! (city, start), [city, start])
              | otherwise =
                  Data.List.minimumBy
                    compEntry
                    [addCity (dp Data.Array.! (other, clearBit bitmask other)) city | other <- bitmaskToList bitmask]

{- Example Graphs -}

gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]

gTestN :: Int -> RoadMap
gTestN n = [(show orig, show dest, 1) | orig <- [1..n], dest <- [orig + 1..n]]
