-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec []      = []
halveEvensRec (x:xs)
  | x `mod` 2 == 0  = x `div` 2 : halveEvensRec xs
  | otherwise       = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ []       = []
inRangeRec lo hi (x:xs)
  | x >= lo && x <= hi  = x : inRangeRec lo hi xs
  | otherwise           = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec []      = 0
countPositivesRec (x:xs)
  | x > 0     = 1 + countPositivesRec xs
  | otherwise = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositivesRec xs == countPositives xs


-- 4.

multDigitsRec :: String -> Int
multDigitsRec []      = 1
multDigitsRec (x:xs)
  | isDigit x = digitToInt x * multDigitsRec xs
  | otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c xs = f [ (p, e) | (p, e) <- xs, p == c ]
  where
    f []        = c
    f ((_,e):_) = e

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c []          = c
lookUpRec c ((p,e):xs)
  | p == c    = e
  | otherwise = lookUpRec c xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUpRec c k == lookUp c k


-- 6.

encipher :: Int -> Char -> Char
encipher shift c = lookUpRec c $ makeKey shift

-- 7.

normalize :: String -> String
normalize = map toUpper . filter (\x -> isLetter x || isDigit x)


-- 8.

encipherStr :: Int -> String -> String
encipherStr shift = map (encipher shift) . normalize


-- Optional Material
-- =================

-- 9.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [ (e, p) | (p, e) <- xs ]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec []          = []
reverseKeyRec ((p,e):xs)  = (e,p) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKeyRec xs == reverseKey xs


-- 10.

decipher :: Int -> Char -> Char
decipher shift c = lookUpRec c (reverseKey $ makeKey shift)

decipherStr :: Int -> String -> String
decipherStr shift cipher = map (decipher shift) cipher


-- 11.

contains :: String -> String -> Bool
contains str substr = or [ isPrefixOf substr $ drop i str | i <- [0..length str] ]


-- 12.

candidates :: String -> [(Int, String)]
candidates cipher = [ (i, decipherStr i cipher) | i <- [0..25] ]


-- 13.

splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive s
  | length s < 5  = [padWithX 5 s]
  | otherwise     = take 5 s : splitEachFive (drop 5 s)

padWithX :: Int -> String -> String
padWithX n s = s ++ replicate (n - length s) 'X'


-- 14.

prop_transpose :: String -> Bool
prop_transpose s = transpose (transpose s') == s'
  where
    s' = splitEachFive s


-- 15.

encrypt :: Int -> String -> String
encrypt shift = concat . transpose . splitEachFive . encipherStr shift


-- 16.

decrypt :: Int -> String -> String
decrypt shift s = decipherStr shift $ concat $ transpose $ splitEachN (length s `div` 5) s

splitEachN :: Int -> String -> [String]
splitEachN _ [] = []
splitEachN n s  = take n s : splitEachN n (drop n s)

shiftGen :: Gen Int
shiftGen = choose (0, 25)

alphabeticCharGen :: Gen Char
alphabeticCharGen = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

letterStringGen :: Gen String
letterStringGen = listOf alphabeticCharGen

prop_encrypt :: Property
prop_encrypt = forAll shiftGen $ \shift ->
                   forAll letterStringGen $ \s ->
                       let
                            normalized  = normalize s
                            split       = concat (splitEachFive normalized)
                            encrypted   = encrypt shift s
                            decrypted   = decrypt shift encrypted
                       in
                         take (length s) decrypted == take (length s) split