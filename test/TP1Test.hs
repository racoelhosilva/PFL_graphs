{- HLINT ignore "Use camelCase" -}

import Test.Hspec
import Test.Hspec.QuickCheck

import TP1

prop_reverseAntiAssociativity :: [Int] -> [Int] -> Bool
prop_reverseAntiAssociativity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_reverseInvolution :: [Int] -> Bool 
prop_reverseInvolution xs = reverse (reverse xs) == xs

main :: IO ()
main = hspec $ do
  describe "reverse" $ do
    prop "reverse is anti-associative"
     prop_reverseAntiAssociativity
    prop "reverse is an involution"
      prop_reverseInvolution
