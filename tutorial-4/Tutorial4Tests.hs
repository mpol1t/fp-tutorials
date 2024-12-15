module Tutorial4Tests (allTests) where

import Tutorial4
import Test.QuickCheck
import Data.Char
import Fixtures
import Types
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

prop_sameString_pos :: String -> Bool
prop_sameString_pos a = sameString a a

prop_sameString_neg :: String -> String -> Property
prop_sameString_neg a b = a /= b ==> not $ sameString a b

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) && prefix substr (map toUpper str)
  where substr = take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str

prop_contains_pos :: String -> String -> Bool
prop_contains_pos a b = contains (a ++ b) b

prop_split :: Char -> String -> String -> Bool
prop_split c sep str =
    let
        Just x = split sep' str
        sep' = c : sep
    in
        reconstruct sep' x `sameString` str

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks

testEmailsFromHTML :: Bool
testEmailsFromHTML = emailsFromHTML testHTML == testAddrBook

allTests :: TestTree
allTests = testGroup "All Tests"
    [ testProps
    , testUnits
    ]

-- Test properties
testProps :: TestTree
testProps = testGroup "Properties"
  [ QC.testProperty "sameString - positive" prop_sameString_pos
  , QC.testProperty "sameString - negative" prop_sameString_neg
  , QC.testProperty "prefix - positive" prop_prefix_pos
  , QC.testProperty "prefix - negative" prop_prefix_neg
  , QC.testProperty "contains - positive" prop_contains_pos
  ]

-- Example unit test group
testUnits :: TestTree
testUnits = testGroup "Unit Tests"
  [ testCase "Links from HTML" $
      linksFromHTML testHTML @?= testLinks
  , testCase "Emails from HTML" $
      emailsFromHTML testHTML @?= testAddrBook
  ]