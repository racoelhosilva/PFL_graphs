# Functional and Logical Programming (PFL) Project 1

## Group T06_G09

| Name                                           | E-mail            | Contribution |
| ---------------------------------------------- | ----------------- | -----------: |
| Bruno Ricardo Soares Pereira de Sousa Oliveira | up202208700@up.pt |          50% |
| Rodrigo Albergaria Coelho e Silva              | up202205188@up.pt |          50% |

### Tasks Developed

- Bruno Ricardo Soares Pereira de Sousa Oliveira
  - Implementation of the Bitmask and Heap structures
  - Development of main and auxiliary functions
  - Test of the functions with properties and descriptions
  - Documentation of the code
- Rodrigo Albergaria Coelho e Silva
  - Implementation of the Set and Map structures
  - Development of main and auxiliary functions
  - Test of the functions with properties and descriptions
  - Documentation of the code

## Algorithm Explanations

### Dijkstra's algorithm: `shortestPath`

<!-- Explanation of how the shortestPath function was implemented, including a justification of why certain auxiliary data structures were selected and used, and a description of the algorithm(s) used. -->


### Held-Karp's algorithm: `travelSales`



## Testing

For this project, we also used Quickcheck to develop tests for all of the data structures used based on their properties.

Besides the data structures we also implemented tests for all of the functions using both unit tests based on the expected results for the example graphs (gTest1, gTest2 and gTest3), as well as properties for each of them.

Finally, the test were created with arbitrary values used from generators we developed for Cities, Edges, Paths and Roadmaps.

Some examples of the tests developed are shown below:

```haskell
{- ... -}

newtype GoodRoadMap = GoodRoadMap RoadMap deriving (Show, Eq)

{- ... -}

instance Arbitrary GoodRoadMap where
  arbitrary = do
    rawMap <- listOf (arbitrary :: Gen GoodEdge)
    return $ GoodRoadMap $ removeDuplicateEdges $ map coerce rawMap

  shrink (GoodRoadMap roadMap) = [GoodRoadMap $ removeDuplicateEdges $ map coerce edges | edges <- shrink $ map GoodEdge roadMap]

{- ... -}

prop_travelSalesSameEnds :: GoodRoadMap -> Property
prop_travelSalesSameEnds (GoodRoadMap roadMap) = let circuit = travelSales roadMap
  in not (null circuit) ==> head circuit == last circuit

prop_travelSalesPassesEachCityOnce :: GoodRoadMap -> Property
prop_travelSalesPassesEachCityOnce (GoodRoadMap roadMap) = let
  circuit = travelSales roadMap
  mapCities = cities roadMap
  in not (null circuit) ==> length circuit == length mapCities + 1 && sortUnique (nub circuit) == sortUnique mapCities

{- ... -}

main :: IO ()
main = hspec $ do

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
```

## References

+ Fethi Rabhi and Guy Lapalme. *Algorithms: a functional programming approach.* Addison-Wesley, 2nd edition, 1999.
