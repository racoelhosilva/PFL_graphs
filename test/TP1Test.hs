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
  orig <- goodCity
  dest <- goodCity
  dist <- arbitrary `suchThat` (> 0)
  return (orig, dest, dist)

goodRoadMap :: Gen RoadMap 
goodRoadMap = listOf goodEdge


-- Property-based tests
prop_reverseAntiAssociativity :: [Int] -> [Int] -> Bool
prop_reverseAntiAssociativity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_reverseInvolution :: [Int] -> Bool 
prop_reverseInvolution xs = reverse (reverse xs) == xs


prop_citiesWithoutDuplicates :: Property
prop_citiesWithoutDuplicates = forAll goodRoadMap $ \roadMap -> allUnique $ cities roadMap where
  allUnique [] = True
  allUnique (x:xs) = x `notElem` xs && allUnique xs

prop_areAdjacentCommutativity :: Property
prop_areAdjacentCommutativity =
  forAll goodRoadMap $ \roadMap ->
  forAll goodCity $ \city1 ->
  forAll goodCity $ \city2 ->
    areAdjacent roadMap city1 city2 == areAdjacent roadMap city2 city1

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

    prop "areAdjacent is commutative" $ do
      prop_areAdjacentCommutativity
