{- HLINT ignore "Use camelCase" -}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Maybe
import Data.List

import TP1
import Data.Char


-- Generators

newtype GoodCity = GoodCity City deriving (Show, Eq)
newtype GoodEdge = GoodEdge (City, City, Distance) deriving (Show, Eq)
newtype GoodRoadMap = GoodRoadMap RoadMap deriving (Show, Eq)

instance Arbitrary GoodCity where
  arbitrary = do
    char <- choose ('A', 'I')
    return $ GoodCity [char]

  shrink (GoodCity [char]) = [GoodCity [newChar] | newChar <- ['A'..chr $ ord char - 1]]
  
instance Arbitrary GoodEdge where
  arbitrary = do
    GoodCity orig <- arbitrary
    GoodCity dest <- arbitrary `suchThat` (/= GoodCity orig)
    Positive dist <- arbitrary
    return $ GoodEdge (orig, dest, dist)
  
  shrink (GoodEdge (orig, dest, dist)) =
    [GoodEdge (newOrig, dest, dist) | GoodCity newOrig <- shrink (GoodCity orig), newOrig /= dest] ++
    [GoodEdge (orig, newDest, dist) | GoodCity newDest <- shrink (GoodCity dest), newDest /= orig] ++
    [GoodEdge (orig, dest, newDist) | newDist <- shrink dist]

unwrapGoodEdge :: GoodEdge -> (City, City, Distance)
unwrapGoodEdge (GoodEdge edge) = edge

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
    return $ GoodRoadMap $ removeDuplicateEdges $ map unwrapGoodEdge rawMap where

  shrink (GoodRoadMap roadMap) =
    [GoodRoadMap $ removeDuplicateEdges $ map unwrapGoodEdge edges | edges <- shrink $ map GoodEdge roadMap]

goodPath :: RoadMap -> Gen Path
goodPath roadMap = do
  mapCities <- shuffle (cities roadMap)
  sublistOf mapCities

forAllGoodPaths :: Testable prop => RoadMap -> (Path -> prop) -> Property
forAllGoodPaths roadMap = forAllShrink (goodPath roadMap) shrink

-- Property-based tests

-- prop_reverseAntiAssociativity :: [Int] -> [Int] -> Bool
-- prop_reverseAntiAssociativity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- prop_reverseInvolution :: [Int] -> Bool
-- prop_reverseInvolution xs = reverse (reverse xs) == xs

prop_emptySetContainsNoElements :: Int -> Bool
prop_emptySetContainsNoElements y = not (containsSet emptySet y)

prop_setContainsInsertedElement :: [Int] -> Int -> Bool
prop_setContainsInsertedElement xs y = let set = foldl insertSet emptySet xs
  in containsSet (insertSet set y) y

prop_setUniqueInsertions :: [Int] -> Int -> Bool
prop_setUniqueInsertions xs n = let set = foldl insertSet emptySet xs
  in insertSet (insertSet set n) n == insertSet set n

prop_setInsertionCommutativity :: [Int] -> Int -> Int -> Bool
prop_setInsertionCommutativity xs y z = let set = foldl insertSet emptySet xs
  in setToList (insertSet (insertSet set y) z) == setToList (insertSet (insertSet set z) y)

prop_setIsBalanced :: [Int] -> Bool
prop_setIsBalanced xs = abs (balanceFactor $ foldl insertSet emptySet xs) <= 1

prop_setIsOrdered :: [Int] -> Bool
prop_setIsOrdered xs = isOrdered res
  where
    isOrdered :: [Int] -> Bool
    isOrdered [] = True
    isOrdered [_] = True
    isOrdered (x:y:xs) = x < y && isOrdered (y:xs)

    res :: [Int]
    res = setToList $ foldl insertSet emptySet xs

prop_entriesWithSameKeyAreEqual :: Int -> Int -> Int -> Bool
prop_entriesWithSameKeyAreEqual k v1 v2 = MEntry k v1 == MEntry k v2

prop_emptyMapContainsNoElements :: Int -> Bool
prop_emptyMapContainsNoElements y = isNothing (lookupMap emptyMap y)

prop_mapContainsInsertedKVPair :: [(Int, Int)] -> Int -> Int -> Bool
prop_mapContainsInsertedKVPair kvs k v = let map = mapFromList kvs
  in unjust (lookupMap (insertMap map k v) k) == v

prop_repeatedMapInsertionsUpdate :: [(Int, Int)] -> Int -> Int -> Int -> Bool
prop_repeatedMapInsertionsUpdate kvs k v1 v2 = let map = mapFromList kvs
  in unjust (lookupMap (insertMap (insertMap map k v1) k v2) k) == v2

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
    isBalanced (HNode _ _ left right) = abs (heapSize left - heapSize right) <= 1
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
        isBalanced (HNode _ _ left right) = abs (heapSize left - heapSize right) <= 1
          && isBalanced left && isBalanced right

prop_citiesWithoutDuplicates :: GoodRoadMap -> Bool
prop_citiesWithoutDuplicates (GoodRoadMap roadMap) = allUnique $ cities roadMap where
  allUnique :: Eq a => [a] -> Bool
  allUnique [] = True
  allUnique (x:xs) = x `notElem` xs && allUnique xs

prop_areAdjacentCommutativity :: GoodRoadMap -> GoodCity -> GoodCity -> Bool
prop_areAdjacentCommutativity (GoodRoadMap roadMap) (GoodCity city1) (GoodCity city2) =
  areAdjacent roadMap city1 city2 == areAdjacent roadMap city2 city1

prop_distanceCommutativity :: GoodRoadMap -> GoodCity -> GoodCity -> Bool
prop_distanceCommutativity (GoodRoadMap roadMap) (GoodCity city1) (GoodCity city2) =
  distance roadMap city1 city2 == distance roadMap city2 city1

prop_distanceAdjacency :: GoodRoadMap -> GoodCity -> GoodCity -> Bool
prop_distanceAdjacency (GoodRoadMap roadMap) (GoodCity city1) (GoodCity city2) =
  let adjacent = areAdjacent roadMap city1 city2
    in case distance roadMap city1 city2 of
      Nothing -> not adjacent
      Just _  -> adjacent

prop_adjacentDistance :: GoodRoadMap -> GoodCity -> Bool
prop_adjacentDistance (GoodRoadMap roadMap) (GoodCity city1) =
  let adjacents = adjacent roadMap city1
    in all (\(city2, dist) -> distance roadMap city1 city2 == Just dist) adjacents

prop_pathDistanceDistributivity :: GoodRoadMap -> GoodCity -> Property
prop_pathDistanceDistributivity (GoodRoadMap roadMap) (GoodCity middleCity) =
  forAllGoodPaths roadMap $ \path1 ->
  forAllGoodPaths roadMap $ \path2 ->
    pathDistance roadMap (path1 ++ [middleCity] ++ path2) == (sum <$> sequence [pathDistance roadMap (path1 ++ [middleCity]), pathDistance roadMap ([middleCity] ++ path2)])


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
      pathDistance gTest1 ["7"] `shouldBe` Just 0  -- TODO: see this
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
