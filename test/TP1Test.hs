{- HLINT ignore "Use camelCase" -}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Maybe
import Data.List

import TP1
import Data.Char
import Data.Coerce (coerce)
import Data.Functor


-- Generators

newtype GoodCity = GoodCity City deriving (Show, Eq)
newtype GoodEdge = GoodEdge (City, City, Distance) deriving (Show, Eq)
newtype GoodRoadMap = GoodRoadMap RoadMap deriving (Show, Eq)
newtype GoodPath = GoodPath Path deriving (Show, Eq)

instance Arbitrary GoodCity where
  arbitrary = do
    char <- choose ('A', 'I')
    return $ GoodCity [char]

  shrink (GoodCity [char]) = [GoodCity [newChar] | newChar <- ['A'..chr $ ord char - 1]]

goodCityBesides :: City -> Gen GoodCity
goodCityBesides other = do
  char <- elements [x | x <- ['A'..'I'], x /= head other]
  return $ GoodCity [char]

instance Arbitrary GoodEdge where
  arbitrary = do
    GoodCity orig <- arbitrary
    GoodCity dest <- goodCityBesides orig
    Positive dist <- arbitrary
    return $ GoodEdge (orig, dest, dist)

  shrink (GoodEdge (orig, dest, dist)) =
    [GoodEdge (newOrig, dest, dist) | GoodCity newOrig <- shrink (GoodCity orig), newOrig /= dest] ++
    [GoodEdge (orig, newDest, dist) | GoodCity newDest <- shrink (GoodCity dest), newDest /= orig] ++
    [GoodEdge (orig, dest, newDist) | Positive newDist <- shrink (Positive dist)]

sameEdge :: (City, City, Distance) -> (City, City, Distance) -> Bool
sameEdge (orig, dest, _) (orig', dest', _) = (orig, dest) == (orig', dest') || (orig, dest) == (dest', orig')

removeDuplicateEdges :: RoadMap -> RoadMap
removeDuplicateEdges [] = []
removeDuplicateEdges (edge:edges)
  | any (sameEdge edge) edges = removeDuplicateEdges edges
  | otherwise                 = edge : removeDuplicateEdges edges

instance Arbitrary GoodRoadMap where
  arbitrary = do
    rawMap <- listOf (arbitrary :: Gen GoodEdge)
    return $ GoodRoadMap $ removeDuplicateEdges $ map coerce rawMap

  shrink (GoodRoadMap roadMap) = [GoodRoadMap $ removeDuplicateEdges $ map coerce edges | edges <- shrink $ map GoodEdge roadMap]

instance Arbitrary GoodPath where
  arbitrary = listOf1 (arbitrary :: Gen GoodCity) <&> (GoodPath . map coerce)

  shrink (GoodPath path) = [GoodPath $ map coerce newPath | newPath <- shrink $ map GoodCity path, not $ null newPath]


forAllCities :: Testable prop => GoodRoadMap -> (GoodCity -> prop) -> Property
forAllCities (GoodRoadMap []) _ = property True
forAllCities (GoodRoadMap roadMap) propFunc = forAllShrink mapCity shrink propFunc
  where
    mapCity :: Gen GoodCity
    mapCity = do
      city <- elements $ cities roadMap
      return $ GoodCity city

forAllEdges :: Testable prop => GoodRoadMap -> (GoodEdge -> prop) -> Property
forAllEdges (GoodRoadMap roadMap) = forAllShrink mapEdge shrink
  where
    mapEdge :: Gen GoodEdge
    mapEdge = elements roadMap <&> GoodEdge

forAllPaths :: Testable prop => GoodRoadMap -> (GoodPath -> prop) -> Property
forAllPaths (GoodRoadMap []) = forAll (return $ GoodPath [])
forAllPaths (GoodRoadMap roadMap) = forAllShrink mapPath shrink
  where
    mapPath :: Gen GoodPath
    mapPath = do
      shuffledCities <- shuffle $ cities roadMap
      newCitySublist <- sublistOf $ tail shuffledCities
      return $ GoodPath $ head shuffledCities : newCitySublist


-- Property-based tests

-- prop_reverseAntiAssociativity :: [Int] -> [Int] -> Bool
-- prop_reverseAntiAssociativity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- prop_reverseInvolution :: [Int] -> Bool
-- prop_reverseInvolution xs = reverse (reverse xs) == xs

prop_emptySetContainsNoElements :: Int -> Bool
prop_emptySetContainsNoElements y = not (setContains emptySet y)

prop_setContainsInsertedElement :: [Int] -> Int -> Bool
prop_setContainsInsertedElement xs y = let set = foldl setInsert emptySet xs
  in setContains (setInsert set y) y

prop_setUniqueInsertions :: [Int] -> Int -> Bool
prop_setUniqueInsertions xs n = let set = foldl setInsert emptySet xs
  in setInsert (setInsert set n) n == setInsert set n

prop_setInsertionCommutativity :: [Int] -> Int -> Int -> Bool
prop_setInsertionCommutativity xs y z = let set = foldl setInsert emptySet xs
  in setToList (setInsert (setInsert set y) z) == setToList (setInsert (setInsert set z) y)

prop_setIsBalanced :: [Int] -> Bool
prop_setIsBalanced xs = abs (setBalanceFactor $ foldl setInsert emptySet xs) <= 1

prop_setIsOrdered :: [Int] -> Bool
prop_setIsOrdered xs = isOrdered res
  where
    isOrdered :: [Int] -> Bool
    isOrdered [] = True
    isOrdered [_] = True
    isOrdered (x:y:xs) = x < y && isOrdered (y:xs)

    res :: [Int]
    res = setToList $ foldl setInsert emptySet xs

prop_entriesWithSameKeyAreEqual :: Int -> Int -> Int -> Bool
prop_entriesWithSameKeyAreEqual k v1 v2 = MEntry k v1 == MEntry k v2

prop_emptyMapContainsNoElements :: Int -> Bool
prop_emptyMapContainsNoElements y = isNothing (mapLookup emptyMap y)

prop_mapContainsInsertedKVPair :: [(Int, Int)] -> Int -> Int -> Bool
prop_mapContainsInsertedKVPair kvs k v = let map = mapFromList kvs
  in unjust (mapLookup (mapInsert map k v) k) == v

prop_repeatedMapInsertionsUpdate :: [(Int, Int)] -> Int -> Int -> Int -> Bool
prop_repeatedMapInsertionsUpdate kvs k v1 v2 = let map = mapFromList kvs
  in unjust (mapLookup (mapInsert (mapInsert map k v1) k v2) k) == v2

prop_mapIsOrdered :: [(Int,Int)] -> Bool
prop_mapIsOrdered kvs = isOrdered res
    where
    isOrdered :: [(Int, Int)] -> Bool
    isOrdered [] = True
    isOrdered [_] = True
    isOrdered (x:y:xs) = fst x < fst y && isOrdered (y:xs)

    res :: [(Int, Int)]
    res = mapToList $ mapFromList kvs

prop_heapIsBalanced :: [Int] -> Bool
prop_heapIsBalanced xs = isBalanced $ foldl heapInsert emptyHeap xs
  where
    isBalanced :: Ord a => Heap a -> Bool
    isBalanced HEmpty = True
    isBalanced (HNode _ _ left right) = abs (heapRank left - heapRank right) <= 1
      && isBalanced left && isBalanced right

prop_heapIsOrdered :: [Int] -> Bool
prop_heapIsOrdered xs = isOrdered $ foldl heapInsert emptyHeap xs
  where
    isOrdered :: Ord a => Heap a -> Bool
    isOrdered HEmpty = True
    isOrdered (HNode _ _ HEmpty HEmpty) = True
    isOrdered (HNode x _ left@(HNode y _ _ _) HEmpty) = x < y && isOrdered left
    isOrdered (HNode x _ HEmpty right@(HNode y _ _ _)) = x < y && isOrdered right
    isOrdered (HNode x _ left@(HNode y _ _ _) right@(HNode z _ _ _)) = x < y && x < z && isOrdered left && isOrdered right

prop_heapInsertNotEmpty :: [Int] -> Int -> Bool
prop_heapInsertNotEmpty xs x = not $ heapIsEmpty $ heapInsert (foldl heapInsert emptyHeap xs) x

prop_heapMinIsExtracted :: NonEmptyList Int -> Bool
prop_heapMinIsExtracted (NonEmpty xs) = let
  heap = foldl heapInsert emptyHeap xs
  min = heapMin heap
  in min == fst (heapPopMin heap) && min == minimum xs

prop_heapBalancedAfterExtraction :: NonEmptyList Int -> Property
prop_heapBalancedAfterExtraction (NonEmpty xs) =
  forAll (choose (0, length xs)) $ \extractions ->
    isBalanced $ iterate (snd . heapPopMin) (foldl heapInsert emptyHeap xs) !! extractions
      where
        isBalanced :: Ord a => Heap a -> Bool
        isBalanced HEmpty = True
        isBalanced (HNode _ _ left right) = abs (heapRank left - heapRank right) <= 1
          && isBalanced left && isBalanced right


prop_citiesWithoutDuplicates :: GoodRoadMap -> Bool
prop_citiesWithoutDuplicates (GoodRoadMap roadMap) = allUnique $ cities roadMap where
  allUnique :: Eq a => [a] -> Bool
  allUnique [] = True
  allUnique (x:xs) = x `notElem` xs && allUnique xs

prop_areAdjacentCommutativity :: GoodRoadMap -> Property
prop_areAdjacentCommutativity goodRoadMap@(GoodRoadMap roadMap) =
  forAllCities goodRoadMap $ \(GoodCity city1) ->
  forAllCities goodRoadMap $ \(GoodCity city2) ->
    areAdjacent roadMap city1 city2 == areAdjacent roadMap city2 city1

prop_distanceCommutativity :: GoodRoadMap -> Property
prop_distanceCommutativity goodRoadMap@(GoodRoadMap roadMap) =
  forAllCities goodRoadMap $ \(GoodCity city1) ->
  forAllCities goodRoadMap $ \(GoodCity city2) ->
    distance roadMap city1 city2 == distance roadMap city2 city1

prop_distanceAdjacency :: GoodRoadMap -> Property
prop_distanceAdjacency goodRoadMap@(GoodRoadMap roadMap) =
  forAllCities goodRoadMap $ \(GoodCity city1) ->
  forAllCities goodRoadMap $ \(GoodCity city2) ->
    let adjacent = areAdjacent roadMap city1 city2
    in case distance roadMap city1 city2 of
      Nothing -> not adjacent
      Just _  -> adjacent

prop_adjacentDistance :: GoodRoadMap -> Property
prop_adjacentDistance goodRoadMap@(GoodRoadMap roadMap) =
  forAllCities goodRoadMap $ \(GoodCity city1) ->
    let adjacents = adjacent roadMap city1
    in all (\(city2, dist) -> distance roadMap city1 city2 == Just dist) adjacents

prop_pathDistanceDistributivity :: GoodRoadMap -> Property
prop_pathDistanceDistributivity goodRoadMap@(GoodRoadMap roadMap) =
  forAllPaths goodRoadMap $ \(GoodPath path1) ->
  forAllCities goodRoadMap $ \ (GoodCity middleCity) ->
  forAllPaths goodRoadMap $ \(GoodPath path2) ->
    pathDistance roadMap (path1 ++ [middleCity] ++ path2) == (sum <$> sequence [pathDistance roadMap (path1 ++ [middleCity]), pathDistance roadMap ([middleCity] ++ path2)])

prop_shortestPathCorrectEnds :: GoodRoadMap -> Property
prop_shortestPathCorrectEnds goodRoadMap@(GoodRoadMap roadMap) =
  forAllCities goodRoadMap $ \(GoodCity city1) ->
  forAllCities goodRoadMap $ \(GoodCity city2) ->
    all (\path -> head path == city1 && last path == city2) $ shortestPath roadMap city1 city2

prop_shortestPathSubstructure :: GoodRoadMap -> Property
prop_shortestPathSubstructure goodRoadMap@(GoodRoadMap roadMap) =
  forAllCities goodRoadMap $ \(GoodCity city1) ->
  forAllCities goodRoadMap $ \(GoodCity city2) ->
    conjoin $ map (\path -> length path > 2 ==> tail path `elem` shortestPath roadMap (path !! 1) city2) $ shortestPath roadMap city1 city2

prop_travelSalesSameEnds :: GoodRoadMap -> Property
prop_travelSalesSameEnds (GoodRoadMap roadMap) = let circuit = travelSales roadMap
  in not (null circuit) ==> head circuit == last circuit 

prop_travelSalesPassesEachCityOnce :: GoodRoadMap -> Property
prop_travelSalesPassesEachCityOnce (GoodRoadMap roadMap) = let
  circuit = travelSales roadMap
  mapCities = cities roadMap
  in not (null circuit) ==> length circuit == length mapCities + 1 && sortUnique (nub circuit) == sortUnique mapCities

-- Auxiliary Functions 

rotate :: [a] -> Int -> [a]
rotate xs n = let (xs1, xs2) = splitAt n (reverse xs)
  in reverse xs1 ++ reverse xs2

sameCircuit :: [City] -> [City] -> Bool
sameCircuit (x:xs) (y:ys) = case elemIndex x ys of
  Just n  -> rotate xs n == ys || rotate xs (length xs - n) == reverse ys
  Nothing -> False

-- Main

main :: IO ()
main = hspec $ do

--  describe "reverse" $ do
--    prop "reverse is anti-associative"
--      prop_reverseAntiAssociativity
--    prop "reverse is an involution"
--      prop_reverseInvolution

-- Test for Data Structures

  describe "Set" $ do
    prop "Empty set contains no elements"
      prop_emptySetContainsNoElements

    prop "Set contains inserted element"
      prop_setContainsInsertedElement

    prop "Insertions are unique"
      prop_setUniqueInsertions

    prop "Insertions are commutative"
      prop_setInsertionCommutativity

    prop "Set is balanced"
      prop_setIsBalanced

    prop "Set is ordered"
      prop_setIsOrdered

  describe "Map" $ do
    prop "Entries with the same key are equal"
      prop_entriesWithSameKeyAreEqual

    prop "Empty Map contains no elements"
      prop_emptyMapContainsNoElements

    prop "Map contains inserted Key Value pair"
      prop_mapContainsInsertedKVPair

    prop "Repeated insertions result in updates"
      prop_repeatedMapInsertionsUpdate

    prop "Map is ordered"
      prop_mapIsOrdered

  describe "Heap" $ do
    describe "insert" $ do
      prop "Heap is balanced"
        prop_heapIsBalanced

      prop "Heap is ordered"
        prop_setIsOrdered

      prop "Heap not empty after insert"
        prop_heapInsertNotEmpty

      prop "Heap min is extracted"
        prop_heapMinIsExtracted

      prop "Heap balanced after extraction"
        prop_heapBalancedAfterExtraction

-- Tests for Functions

  describe "cities" $ do
    it "Right cities are listed" $ do
      sort (cities gTest1) `shouldBe` map show [0..8]
      sort (cities gTest2) `shouldBe` map show [0..3]
      sort (cities gTest3) `shouldBe` map show [0..3]

    prop "cities do not list duplicated"
      prop_citiesWithoutDuplicates

  describe "areAdjacent" $ do
    it "Right pairs are accepted" $ do
      areAdjacent gTest1 "7" "6" `shouldBe` True
      areAdjacent gTest1 "3" "2" `shouldBe` True
      areAdjacent gTest2 "3" "1" `shouldBe` True
      areAdjacent gTest3 "2" "3" `shouldBe` True

    it "Wrong pairs are rejected" $ do
      areAdjacent gTest1 "6" "0" `shouldBe` False
      areAdjacent gTest1 "0" "A" `shouldBe` False
      areAdjacent gTest3 "0" "2" `shouldBe` False

    prop "areAdjacent is commutative"
      prop_areAdjacentCommutativity

  describe "distance" $ do
    it "Right distances are given" $ do
      distance gTest1 "7" "6" `shouldBe` Just 1
      distance gTest1 "3" "2" `shouldBe` Just 7
      distance gTest2 "3" "1" `shouldBe` Just 25
      distance gTest3 "2" "3" `shouldBe` Just 2

    it "Nothing is given for non existing edges" $ do
      distance gTest1 "6" "0" `shouldBe` Nothing
      distance gTest1 "0" "A" `shouldBe` Nothing
      distance gTest3 "0" "2" `shouldBe` Nothing

    prop "distance is commutative"
      prop_distanceCommutativity

    prop "distance relates to areAdjacent"
      prop_distanceAdjacency

  describe "adjacent" $ do
    it "Right adjacent cities are given" $ do
      sort (adjacent gTest1 "7") `shouldBe` [("0", 8), ("1", 11), ("6", 1), ("8", 7)]
      sort (adjacent gTest1 "3") `shouldBe` [("2", 7), ("4", 9), ("5", 14)]
      sort (adjacent gTest2 "3") `shouldBe` [("0", 20), ("1", 25), ("2", 30)]
      sort (adjacent gTest3 "2") `shouldBe` [("3", 2)]

    prop "Adjacent cities have right distances"
      prop_adjacentDistance

  describe "pathDistance" $ do
    it "Right path distances are given" $ do
      pathDistance gTest1 ["7", "6", "5", "2", "1"] `shouldBe` Just 15
      pathDistance gTest2 ["0", "1", "2", "3"] `shouldBe` Just 75
      pathDistance gTest3 ["0", "1"] `shouldBe` Just 4

    it "Nothing is returned for disconnected paths" $ do
      pathDistance gTest1 ["7", "6", "2", "1"] `shouldBe` Nothing
      pathDistance gTest3 ["0", "1", "2"] `shouldBe` Nothing

    it "Nothing is returned for small paths" $ do
      pathDistance gTest1 ["7"] `shouldBe` Just 0
      pathDistance gTest1 [] `shouldBe` Nothing

    prop "Distance of concatenation equals sum of distances" $ do
      prop_pathDistanceDistributivity

  describe "rome" $ do
    it "City with most roads is returned" $ do
      rome gTest1 `shouldBe` ["2", "5", "7"]
      rome gTest2 `shouldBe` ["0", "1", "2", "3"]
      rome gTest3 `shouldBe` ["0", "1", "2", "3"]

    it "Rome of empty graph is empty list" $ do
      rome [] `shouldBe` []

  describe "isStronglyConnected" $ do
    it "Detects connected maps" $ do
      isStronglyConnected gTest1 `shouldBe` True
      isStronglyConnected gTest2 `shouldBe` True

    it "Detects unconnected maps" $ do
      isStronglyConnected gTest3 `shouldBe` False

  describe "shortestPath" $ do
    it "Calculates right shortest path" $ do
      shortestPath gTest1 "7" "2" `shouldBe` [["7", "6", "5", "2"]]
      shortestPath gTest2 "1" "2" `shouldBe` [["1", "0", "2"]]
      shortestPath gTest3 "0" "1" `shouldBe` [["0", "1"]]

    it "Calculates multiple shortest paths" $ do
      sort (shortestPath gTest1 "7" "8") `shouldBe` [["7", "6", "8"], ["7", "8"]]

    it "Checks if no path exists" $ do
      shortestPath gTest3 "0" "2" `shouldBe` []

    prop "Shortest paths start and end on the right cities"
      prop_shortestPathCorrectEnds

    prop "Shortest paths respect optimal substructure"
      prop_shortestPathSubstructure

  describe "travelSales" $ do
    it "Determines correct circuit" $ do
      travelSales gTest1 `shouldSatisfy` sameCircuit ["0", "1", "2", "3", "4", "5", "6", "8", "7", "0"]
      travelSales gTest2 `shouldSatisfy` sameCircuit ["0", "1", "3", "2", "0"]

    it "Checks if no hamiltonian circuit exists" $ do
      travelSales gTest3 `shouldBe` []

    prop "Circuit has the same ends" $ do
      prop_travelSalesSameEnds

    prop "Circuit passes through each city exactly once" $ do
      prop_travelSalesPassesEachCityOnce

    
