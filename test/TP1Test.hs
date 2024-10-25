{- HLINT ignore "Use camelCase" -}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.List 

import TP1


-- Generators

goodCity :: Gen City
goodCity = elements [[c] | c <- ['A'..'I']]

goodEdge :: Gen (City, City, Distance)
goodEdge = do
  orig <- goodCity;
  dest <- goodCity `suchThat` (/= orig);
  dist <- arbitrary `suchThat` (> 0);
  return (orig, dest, dist);

goodPath :: RoadMap -> Gen Path
goodPath roadMap = shuffle (cities roadMap) >>= sublistOf
  

goodRoadMap :: Gen RoadMap 
goodRoadMap = do 
  rawMap <- listOf goodEdge;
  return $ removeDuplicates rawMap where
    sameEdge :: (City, City, Distance) -> (City, City, Distance) -> Bool 
    sameEdge (orig, dest, _) (orig', dest', _) = (orig, dest) == (orig', dest') || (orig, dest) == (dest', orig')

    removeDuplicates :: RoadMap -> RoadMap
    removeDuplicates [] = []
    removeDuplicates (edge:edges) 
      | any (sameEdge edge) edges = removeDuplicates edges
      | otherwise                 = edge : removeDuplicates edges


-- Property-based tests

prop_reverseAntiAssociativity :: [Int] -> [Int] -> Bool
prop_reverseAntiAssociativity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_reverseInvolution :: [Int] -> Bool 
prop_reverseInvolution xs = reverse (reverse xs) == xs


prop_citiesWithoutDuplicates :: Property
prop_citiesWithoutDuplicates = forAll goodRoadMap $ \roadMap -> allUnique $ cities roadMap where
  allUnique :: Eq a => [a] -> Bool 
  allUnique [] = True
  allUnique (x:xs) = x `notElem` xs && allUnique xs

prop_areAdjacentCommutativity :: Property
prop_areAdjacentCommutativity =
  forAll goodRoadMap $ \roadMap ->
  forAll goodCity $ \city1 ->
  forAll goodCity $ \city2 ->
    areAdjacent roadMap city1 city2 == areAdjacent roadMap city2 city1

prop_distanceCommutativity :: Property
prop_distanceCommutativity =
  forAll goodRoadMap $ \roadMap ->
  forAll goodCity $ \city1 ->
  forAll goodCity $ \city2 ->
    distance roadMap city1 city2 == distance roadMap city2 city1

prop_distanceAdjacency :: Property
prop_distanceAdjacency =
  forAll goodRoadMap $ \roadMap ->
  forAll goodCity $ \city1 ->
  forAll goodCity $ \city2 ->
    let adjacent = areAdjacent roadMap city1 city2
    in case distance roadMap city1 city2 of
      Nothing -> not adjacent
      Just _  -> adjacent

prop_adjacentDistance :: Property
prop_adjacentDistance =
  forAll goodRoadMap $ \roadMap ->
  forAll goodCity $ \city1 ->
    let adjacents = adjacent roadMap city1
    in all (\(city2, dist) -> distance roadMap city1 city2 == Just dist) adjacents

prop_pathDistanceDistributivity :: Property
prop_pathDistanceDistributivity =
  forAll goodRoadMap $ \roadMap ->
  forAll (goodPath roadMap) $ \path1 ->
  forAll goodCity $ \middleCity ->
  forAll (goodPath roadMap) $ \path2 ->
    pathDistance roadMap (path1 ++ [middleCity] ++ path2) == (sum <$> sequence [pathDistance roadMap (path1 ++ [middleCity]), pathDistance roadMap ([middleCity] ++ path2)])


-- Main

main :: IO ()
main = hspec $ do
  describe "reverse" $ do
    prop "reverse is anti-associative"
      prop_reverseAntiAssociativity
    prop "reverse is an involution"
      prop_reverseInvolution

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