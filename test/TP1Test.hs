import Control.Monad (when)
import Test.QuickCheck (quickCheckResult)
import Test.QuickCheck.Test (isSuccess)
import System.Exit (exitFailure)

import TP1

prep_reverseAntiAssociativity :: [Int] -> [Int] -> Bool
prep_reverseAntiAssociativity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prep_reverseInvolution :: [Int] -> Bool 
prep_reverseInvolution xs = reverse (reverse xs) == xs

main :: IO()
main = do
  let tests = [ quickCheckResult prep_reverseAntiAssociativity
              , quickCheckResult prep_reverseInvolution
              ]
  success <- fmap (all isSuccess) . sequence $ tests
  when (not success) $ exitFailure
