-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)

-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs, x `mod` 2 == 0 ]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, x >= lo, x <= hi ]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = and [ y >= lo && y <= hi | y <- inRange lo hi xs ]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [ x | x <- xs, x > 0 ]

prop_countPositives :: [Int] -> Bool
prop_countPositives xs = c >= 0 && c <= length xs
  where c = countPositives xs


-- 4. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum [ discount x | x <- prices, discount x <= 19900 ]
  where discount y = round (fromIntegral y * 0.9)

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [ x | x <- xs, x > 0]



-- 5. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits s = product [ digitToInt x | x <- s, isDigit x ]

countDigits :: String -> Int
countDigits s = length [ x | x <- s, isDigit x ]

prop_multDigits :: String -> Bool
prop_multDigits s = multDigits s <= 9 ^ (countDigits s)


-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise []     = []
capitalise (x:xs) = toUpper x : [ toLower y | y <- xs ]

letterGen :: Gen Char
letterGen = elements (['a'..'z'] ++ ['A'..'Z'])

letterStringGen :: Gen String
letterStringGen = listOf letterGen

prop_capitalise :: Property
prop_capitalise = forAll letterStringGen $ \s ->
    let
      capitalised = capitalise s
    in
      case capitalised of
        []     -> True
        (x:xs) -> isUpper x && all isLower xs


-- 7. title

lowercase :: String -> String
lowercase xs = [ toLower x | x <- xs ]

prop_lowercase :: Property
prop_lowercase = forAll letterStringGen $ \s -> all isLower $ lowercase s

-- List-comprehension version
title :: [String] -> [String]
title []      = []
title (x:xs)  = capitalise x : [ f y | y <- xs ]
  where
    f z
      | length z >= 4 = capitalise z
      | otherwise     = lowercase z

letterStringListGen :: Gen [String]
letterStringListGen = listOf letterStringGen

prop_title :: Property
prop_title = forAll letterStringListGen $ \s -> f $ title s
  where
    f []      = True
    f (x:xs)  = isTitle x && and [ checkCase y | y <- xs ]

    checkCase xs
      | length xs >= 4  = isTitle xs
      | otherwise       = isLowerCase xs

    isTitle []      = True
    isTitle (x:xs)  = isUpper x && and [ isLower y | y <- xs ]

    isLowerCase xs = and [ isLower x | x <- xs ]


-- 8. signs

sign :: Int -> Maybe Char
sign 0  = Just '0'
sign i
  | i >= 1    && i <= 9     = Just '+'
  | i >= (-9) && i <= (-1)  = Just '-'
  | otherwise               = Nothing

signs :: [Int] -> String
signs xs = [ c | Just c <- [ sign x | x <- xs ] ]


-- 9. score

score :: Char -> Int
score x
  | isLetter x  = 1 + f (toLower x) + g x
  | otherwise   = 0
    where
      f 'a' = 1
      f 'e' = 1
      f 'i' = 1
      f 'o' = 1
      f 'u' = 1
      f _   = 0

      g y | isUpper y = 1
          | otherwise = 0

totalScore :: String -> Int
totalScore xs = product [ score x | x <- xs, isLetter x ]

prop_totalScore_positive :: String -> Bool
prop_totalScore_positive xs = totalScore xs >= 1


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ w | w <- words, length w == len && w !! pos == letter ]


-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search xs goal = [ i | (x, i) <- zip xs [0..length xs], x == goal ]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = all (==goal) [ str !! i | i <- search str goal ]


-- 10. contains

contains :: String -> String -> Bool
contains str substr = or [ isPrefixOf substr $ drop i str | i <- [0..length str] ]

-- Depending on the property you want to test, you might want to change the type signature
prop_contains_pos :: String -> String -> Bool
prop_contains_pos str1 str2 = contains (str1 ++ str2) str2

-- Custom generator for pairs of strings where the second is not a substring of the first
pairStringGen :: Gen (String, String)
pairStringGen = do
    str1 <- letterStringGen
    str2 <- letterStringGen `suchThat` (not . (`isInfixOf` str1))
    return (str1, str2)

prop_contains_neg :: Property
prop_contains_neg = forAll pairStringGen $ \(str1, str2) -> not $ contains str1 str2