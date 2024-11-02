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
    -- | All cities in the roadmap.
    mapCities :: [City]
    mapCities = cities roadMap

    -- | List of list of city/distance pairs for each city in the roadmap, in order.
    adjs :: [[(City, Distance)]]
    adjs = sortedAdjacents (Data.List.sort roadMap) mapCities

    -- | List of list of city/distance pairs for each city in the reverse roadmap (all edges flipped), in order.
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
    -- | Inserts an edge into the empty.
    --
    --   Arguments:
    --     * AdjMap: representation of the graph.
    --     * (City, City, Distance): edge to insert into the graph.
    --
    --   Returns:
    --     * AdjMap: resulting representation of the graph.
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
    -- | Number of cities in the graph.
    numCities :: Int
    numCities = mapSize indexMap

    -- | Index of the city, according to the indexMap defined.
    --
    --   Arguments:
    --     * City: city to retrieve the index.
    --
    --   Returns:
    --     * Int: index of the city in the map.
    getIndex :: City -> Int
    getIndex city = unjust $ mapLookup indexMap city

    -- | Edge list represented as a pair of indices and a distance.
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

-- | Set data structure based on an AVL Tree.
data Set a = SEmpty | SNode a (Set a) (Set a) Int
  deriving (Show, Eq)

-- | Returns an empty set.
emptySet :: Set a
emptySet = SEmpty

-- | Returns the height of a set.
--
--   Arguments:
--     * Set a: set to obtain the height.
--
--   Returns:
--     * Int: height of the set.
setHeight :: Set a -> Int
setHeight SEmpty          = 0
setHeight (SNode _ _ _ h) = h

-- | Recalculates the heights of a set node.
--
--   Arguments:
--     * Set a: set node to recalculate height.
--
--   Returns:
--     * Set a: resulting set with the recalculated height.
setUpdateHeight :: Set a -> Set a
setUpdateHeight SEmpty = SEmpty
setUpdateHeight (SNode v l r _) = SNode v l r (1 + max (setHeight l) (setHeight r))

-- | Balance factor of a set.
--
--   Arguments:
--     * Set a: set node to recalculate height.
--
--   Returns:
--     * Int: Balance factor between the left and right children.
setBalanceFactor :: Set a -> Int
setBalanceFactor SEmpty          = 0
setBalanceFactor (SNode _ l r _) = setHeight l - setHeight r

-- | Performs a right rotation on the set node.
--
--   Arguments:
--     * Set a: set to perform the right rotation.
--
--   Returns:
--     * Set a: resulting set after the rotation.
setRotateRight :: Set a -> Set a
setRotateRight (SNode y (SNode x l lx h2) r h1) = setUpdateHeight $ SNode x l (setUpdateHeight (SNode y lx r undefined)) undefined
setRotateRight s = s

-- | Performs a left rotation on the set node.
--
--   Arguments:
--     * Set a: set to perform the left rotation.
--
--   Returns:
--     * Set a: resulting set after the rotation.
setRotateLeft :: Set a -> Set a
setRotateLeft (SNode x l (SNode y rx r h2) h1) = setUpdateHeight $ SNode y (setUpdateHeight (SNode x l rx undefined)) r undefined
setRotateLeft s = s

-- | Balances the current set based on left and right rotations.
--
--   Arguments:
--     * Set a: set to perform the balance operation.
--
--   Returns:
--     * Set a: resulting set after the balance operation.
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

-- | Inserts a new element into the set. If the element exists, replaces it (useful for Map implementation).
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Set a: set to perform the insert.
--     * a: element to insert into the set.
--
--   Returns:
--     * Set a: resulting node of the element inserted.
setInsert :: (Ord a) => Set a -> a -> Set a
setInsert SEmpty newVal = SNode newVal SEmpty SEmpty 1
setInsert (SNode val l r h) newVal
  | val > newVal = setBalance (SNode val (setInsert l newVal) r h)
  | val < newVal = setBalance (SNode val l (setInsert r newVal) h)
  | otherwise = SNode newVal l r h

-- | Checks whether the set contains a specific element.
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Set a: set to check if element is contained.
--     * a: element to check.
--
--   Returns:
--     * Bool: True if the set contains the element, False otherwise.
setContains :: (Ord a) => Set a -> a -> Bool
setContains SEmpty _ = False
setContains (SNode val l r h) target
  | val > target = setContains l target
  | val < target = setContains r target
  | otherwise = True

-- | Searches the set for the element.
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Set a: set to search for the element.
--     * a: element to search.
--
--   Returns:
--     * Set a: Node of the element searched, empty node otherwise.
setSearch :: (Ord a) => Set a -> a -> Set a
setSearch SEmpty _ = SEmpty
setSearch (SNode val l r h) target
  | val > target = setSearch l target
  | val < target = setSearch r target
  | otherwise = SNode val l r h

-- | Size of the set (number of elements).
--
--   Arguments:
--     * Set a: set to retrieve the size.
--
--   Returns:
--     * Int: Size of the set.
setSize :: Set a -> Int
setSize SEmpty          = 0
setSize (SNode _ l r _) = 1 + setSize l + setSize r

-- | Converts the set to a list.
--
--   Arguments:
--     * Set a: set to convert to list.
--
--   Returns:
--     * [a]: List resulting of the set conversion
setToList :: Set a -> [a]
setToList SEmpty          = []
setToList (SNode v l r _) = setToList l ++ [v] ++ setToList r

{- Data Structures: Map -}

-- | Key value pair for the map.
data MEntry k v = MEntry k v
  deriving (Show)

instance (Eq k) => Eq (MEntry k v) where
  -- | Equality (==) operator for the Entry type.
  (MEntry k1 _) == (MEntry k2 _) = k1 == k2

instance (Ord k) => Ord (MEntry k v) where
  -- | Compare operator for the Entry type.
  (MEntry k1 _) `compare` (MEntry k2 _) = k1 `compare` k2

-- | Map data structure based on the Set and Entry types.
newtype (Ord k) => Map k v = Map (Set (MEntry k v))
  deriving (Show, Eq)

emptyMap :: (Ord k) => Map k v
emptyMap = Map emptySet

-- | Insert a key and value into a map. Replaces the value if the key already exists (due to the set implementation)
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Map k v: map to insert the elements.
--     * k: key of the pair.
--     * v: value of the pair.
--
--   Returns:
--     * Map k v: Resulting map after the insertion.
mapInsert :: (Ord k) => Map k v -> k -> v -> Map k v
mapInsert (Map s) key value = Map (setInsert s (MEntry key value))

-- | Retrieves the value of a key in a map, it the pair exists.
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Map k v: map to retrieve the element.
--     * k: key of the pair.
--
--   Returns:
--     * Maybe v: Just v in case the key value pair exists, Nothing if the key isn't matched.
mapLookup :: (Ord k) => Map k v -> k -> Maybe v
mapLookup (Map s) key =
  case setSearch s (MEntry key undefined) of
    SEmpty                   -> Nothing
    SNode (MEntry _ v) _ _ _ -> Just v

-- | Retrieves the size of the map (number of entries).
--
--   Arguments:
--     * Map k v: map to retrieve the size.
--
--   Returns:
--     * Int: Size of the map.
mapSize :: (Ord k) => Map k v -> Int
mapSize (Map s) = setSize s

-- | Creates a map from a list of key value pairs.
--
--   Arguments:
--     * [(k,v)]: list of key value pairs to be inserted.
--
--   Returns:
--     * Map k v: the map with the key value pairs as entries.
mapFromList :: (Ord k) => [(k, v)] -> Map k v
mapFromList []                     = emptyMap
mapFromList ((key, val) : subList) = mapInsert (mapFromList subList) key val

-- | Converts the map of key value entries into a list.
--
--   Arguments:
--     * Map k v: the map with the key value pairs as entries.
--
--   Returns:
--     * [(k,v)]: list of key value pairs.
mapToList :: (Ord k) => Map k v -> [(k, v)]
mapToList (Map SEmpty) = []
mapToList (Map (SNode (MEntry k v) l r _)) = mapToList (Map l) ++ [(k, v)] ++ mapToList (Map r)

{- Data Structures: Binary Heap -}

-- | Implementation of a binary min heap.
--
-- The heap is represented as a leftist tree, storing its value, rank and subtrees for each node.
--
--  Arguments:
--   * a: Type of the values stored in the heap.
data (Ord a) => Heap a = HNode a Int (Heap a) (Heap a) | HEmpty
  deriving (Show, Eq)

-- | Returns an empty heap.
emptyHeap :: Heap a
emptyHeap = HEmpty

-- | Returns the rank of a heap node.
--
--   Arguments:
--     * Heap a: heap to obtain the rank.
heapRank :: (Ord a) => Heap a -> Int
heapRank HEmpty          = 0
heapRank (HNode _ r _ _) = r

-- | Returns the minimum value in the heap.
--
--   Efficiency:
--     * Time Complexity: O(1)
--
--   Arguments:
--     * Heap a: heap to obtain the minimum value.
heapMin :: (Ord a) => Heap a -> a
heapMin HEmpty            = error "Cannot pop empty heap"
heapMin (HNode min _ _ _) = min

-- | Builds a new heap, given two heaps and a top value.
--
--   This function verifies that the resulting heap remains a leftist tree,
--   by comparing the rank of the two subtrees, and updates the rank
--   of the new heap accordingly.
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * a: top value of the heap.
--     * Heap a: first heap.
--     * Heap a: second heap.
--
--   Returns:
--     * Heap a: resulting heap.
heapBuild :: (Ord a) => a -> Heap a -> Heap a -> Heap a
heapBuild x h1 h2
  | heapRank h1 >= heapRank h2 = HNode x (heapRank h2 + 1) h1 h2
  | otherwise = HNode x (heapRank h1 + 1) h2 h1

-- | Merges two heaps into a single heap.
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Heap a: first heap to merge.
--     * Heap a: second heap to merge.
heapMerge :: (Ord a) => Heap a -> Heap a -> Heap a
heapMerge h HEmpty = h
heapMerge HEmpty h = h
heapMerge h1@(HNode x _ l1 r1) h2@(HNode y _ l2 r2)
  | x <= y = heapBuild x l1 (heapMerge r1 h2)
  | otherwise = heapBuild y l2 (heapMerge h1 r2)

-- | Inserts a new value into the heap.
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Heap a: heap to insert the value.
--     * a: value to insert.
--
--   Returns:
--     * Heap a: resulting heap.
heapInsert :: (Ord a) => Heap a -> a -> Heap a
heapInsert heap x = heapMerge (HNode x 1 HEmpty HEmpty) heap

-- | Removes the minimum value from the heap.
--
--   Efficiency:
--     * Time Complexity: O(log N)
--
--   Arguments:
--     * Heap a: heap to remove the minimum value.
--
--   Returns:
--     * (a, Heap a): Pair of the minimum value and the popped heap.
heapPopMin :: (Ord a) => Heap a -> (a, Heap a)
heapPopMin HEmpty          = error "Cannot pop empty heap"
heapPopMin (HNode x _ r l) = (x, heapMerge r l)

-- | Checks if the heap is empty.
--
--   Efficiency:
--     * Time Complexity: O(1)
--
--   Arguments:
--     * Heap a: heap to check if it is empty.
heapIsEmpty :: (Ord a) => Heap a -> Bool
heapIsEmpty HEmpty = True
heapIsEmpty _      = False

{- Auxiliary Functions -}

-- | Converts a list of items into a map of items to Int.
--
--   Arguments:
--     * [a]: list of items to be converted.
--
--   Returns:
--     * Map a Int: Map of the items to Int values.
mapToIndexes :: (Ord a) => [a] -> Map a Int
mapToIndexes xs = mapFromList $ zip xs [0..]

-- | Merges two sorted lists into a single sorted list.
--
--   Efficiency:
--     * Time Complexity: O(N + M)
--       and second lists, respectively.
--
--   Arguments:
--     * [a]: sorted list number 1.
--     * [a]: sorted list number 2.
--   Returns:
--     * [a]: Resulting sorted list.
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- | Given an edge of the roadmap, returns the reverse edge.
--
--   Efficiency:
--     * Time Complexity: O(1)
--
--   Arguments:
--     * (City, City, Distance): edge of the roadmap.
--
--   Returns:
--     * (City, City, Distance): Reverse of the edge given.
reverseEdge :: (City, City, Distance) -> (City, City, Distance)
reverseEdge (orig, dest, dist) = (dest, orig, dist)

-- | Given a roadmap, returns the reverse graph.
--
--   Efficiency:
--     * Time Complexity: O(E)
--
--   Arguments:
--     * Roadmap: representation of the graph.
--
--   Returns:
--     * Roadmap: representation of the reverse graph (edge directions are switched).
reverseGraph :: RoadMap -> RoadMap
reverseGraph roadMap = [reverseEdge edge | edge <- roadMap]

-- | Given an ordered roadmap and a list of ordered cities, returns the list of
--   all adjacents (and distances) of the cities in the same order of the
--   argument.
--
--   Efficiency:
--     * Time Complexity: O(E)
--
--   Arguments:
--     * Roadmap: sorted representation of the graph.
--     * [City]: ordered list of cities to retrieve the adjacents.
--
--   Returns:
--     * [[(City, Distance)]]: List of list of adjacents (and distances) of the cities passed.
sortedAdjacents :: RoadMap -> [City] -> [[(City, Distance)]]
sortedAdjacents _ [] = []
sortedAdjacents roadMap (city : cities) = adjacent : sortedAdjacents subRoadMap cities
  where
    adjacent :: [(City, Distance)]
    subRoadMap :: RoadMap
    (adjacent, subRoadMap) = sortedAdjacent roadMap city

    -- | Given an ordered roadmap and a city, returns the pair of sorted adjacents
    --   (and distances) and the rest of the roadmap.
    --
    --   Arguments:
    --     * Roadmap: sorted representation of the graph.
    --     * City: given city to retrieve the adjacents.
    --
    --   Returns:
    --     * ([(City, Distance)], Roadmap): Pair of the sorted adjacents (and distances) and the rest of the roadmap.
    sortedAdjacent :: RoadMap -> City -> ([(City, Distance)], RoadMap)
    sortedAdjacent [] _ = ([], [])
    sortedAdjacent ((orig, dest, dist) : subRoadMap) city
      | orig == city = ((dest, dist) : subAdjacents, finalRoadMap)
      | otherwise = ([], (orig, dest, dist) : subRoadMap)
      where
        (subAdjacents, finalRoadMap) = sortedAdjacent subRoadMap city

-- | Sorts the list and removes all duplicate elements.
--
--   Efficiency:
--     * Time Complexity: O(N log N)
--
--   Arguments:
--     * [a]: initial list.
--
--   Returns:
--     * [a]: Final list, sorted and with a single occurrence for each item.
sortUnique :: (Ord a) => [a] -> [a]
sortUnique xs = unique $ Data.List.sort xs

-- | Removes adjacent duplicate values in a list.
--
--   Efficiency:
--     * Time Complexity: O(N)
--
--   Arguments:
--     * [a]: initial list.
--
--   Returns:
--   * [a]: Final list with single occurrences for each item.
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x1 : x2 : xs)
  | x1 == x2 = unique (x2 : xs)
  | otherwise = x1 : unique (x2 : xs)
-- | Unpacks a Just value, returning the value inside, or an undefined value if the argument is Nothing.
--
--   Arguments:
--     * Maybe a: Maybe value to unpack.
--
--   Returns:
--     * a: Unpacked value in case the argument is a Just. Undefined otherwise.
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
--     * [City]: List of the cities (vertices) in the roadmap .
cities :: RoadMap -> [City]
cities [] = []
cities r = sortUnique $ citySelect r
  where
    -- | Retrieves a list that, for each edge, contains both the start and end cities.
    --
    --   Efficiency:
    --     * Time Complexity: O(E)
    --
    --   Arguments:
    --     * Roadmap: representation of the graph.
    --
    --   Returns:
    --     * [City]: Sorted list of all cities taken from the start or end of the edges in the roadmap.
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
    -- | Checks if the edge connects the two given cities (in either direction).
    --
    --   Arguments:
    --     * (City, City, Distance): edge of the roadmap.
    --
    --   Returns:
    --     * Bool: True if the edge connects them, False otherwise.
    connectsCities :: (City, City, Distance) -> Bool
    connectsCities (orig, dest, _) = (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)

-- | Returns the distance between two cities in a roadmap, if they are adjacent,
--   or Nothing if they are not.
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
distance roadMap city1 city2 = if null matches then Nothing else Just (head matches)
  where
    -- | List of the distances of edges between the given cities (presumably, between length is either 0 or 1)
    matches :: [Distance]
    matches = [dist | (orig, dest, dist) <- roadMap, (orig, dest) == (city1, city2) || (dest, orig) == (city1, city2)]

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
    -- | Adjacency map representation of the roadmap.
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
    visitedSet = dfsVisit emptySet root

    -- | Auxiliary function, that performs a DFS search on the roadmap
    --   Arguments:
    --     * Set City: set of visited cities until this point.
    --     * City: current root city.
    --   Returns:
    --     * Set City: updated set of visited cities, reachable from the root city.
    dfsVisit :: Set City -> City -> Set City
    dfsVisit visitedSet root = getNextVisitedSet (setInsert visitedSet root) adjs
      where
        -- | List of cities adjacent to the current city
        adjs :: [City]
        adjs = map fst (unjust $ mapLookup adjMap root)

        -- | Generates the next visited set of cities.
        --   Arguments:
        --     * Set City: set of visited cities until this point.
        --     * City: city to be analyzed in the current iteration.
        --   Returns:
        --     * Set City: updated set of visited cities.
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
    -- | Adjacency map representation of the roadmap.
    adjMap :: AdjMap
    adjMap = toAdjMap roadMap

    -- | Map that links each city to its predecessors after the Dijkstra algorithm
    pathMap :: Map City Predecessors
    pathMap = dijkstra emptyMap (heapInsert emptyHeap (0, orig, [[orig]]))

    dijkstra :: Map City Predecessors -> Heap DijkstraState -> Map City Predecessors
    dijkstra predMap heap
      | heapIsEmpty heap = predMap
      | otherwise = newMap
      where
        newDist :: Distance
        city :: City
        paths :: [Path]
        restHeap :: Heap DijkstraState
        ((newDist, city, paths), restHeap) = heapPopMin heap

        newMap :: Map City Predecessors
        newMap = case mapLookup predMap city of
          Just (oldDist, oldPaths) ->
            case newDist `compare` oldDist of
              EQ -> dijkstra (mapInsert predMap city (newDist, paths ++ oldPaths)) restHeap
              GT -> if city == dest then predMap else dijkstra predMap restHeap
          Nothing -> dijkstra (mapInsert predMap city (newDist, paths)) updatedHeap

        updatedHeap :: Heap DijkstraState
        updatedHeap = foldr insertState restHeap adjs
          where
            adjs :: [(City, Distance)]
            adjs = unjust $ mapLookup adjMap city

            insertState :: (City, Distance) -> Heap DijkstraState -> Heap DijkstraState
            insertState (neighbor, dist) heap = heapInsert heap (newDist + dist, neighbor, map (neighbor :) paths)

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
    -- | Map that converts cities into Int indexes.
    indexMap :: Map City Int
    indexMap = mapToIndexes $ cities roadMap

    -- | Adjacency matrix representation of the roadmap.
    adjMatrix :: AdjMatrix
    adjMatrix = toAdjMatrix roadMap indexMap

    -- | Reverse of the indexMap, converts Int indexes into cities.
    cityMap :: Map Int City
    cityMap = mapFromList $ map (\(x, y) -> (y, x)) (mapToList indexMap)

    -- | Given an index, returns the corresponding city, according to the cityMap.
    --
    --   Arguments:
    --     * Int: index to access.
    --
    --   Returns:
    --     * City: city associated with that index.
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
