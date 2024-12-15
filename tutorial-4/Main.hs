module Main where

import Tutorial4Tests (allTests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain allTests