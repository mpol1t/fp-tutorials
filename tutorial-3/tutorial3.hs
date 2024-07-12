-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 19-20 Oct.

module Tutorial3 where

import Data.Char
import Test.QuickCheck
import Data.List (nub, transpose, intercalate)



-- 1. Map
-- a.
uppers :: String -> String
uppers = map toUpper

-- b.
doubles :: [Int] -> [Int]
doubles = map (*2)

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds = map (\x -> fromIntegral x / 100)

-- d.
uppers' :: String -> String
uppers' s = [ toUpper x | x <- s ]

prop_uppers :: String -> Bool
prop_uppers s = uppers s == uppers' s



-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

prop_alphas :: String -> Bool
prop_alphas = all isAlpha . alphas

-- b.
rmChar ::  Char -> String -> String
rmChar c = filter (/=c)

-- c.
above :: Int -> [Int] -> [Int]
above n = filter (>n)

prop_above :: Int -> [Int] -> Bool
prop_above n = all (>n) . above n

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(x, y) -> x /= y)

prop_unequals :: [(Int, Int)] -> Bool
prop_unequals = all (\(x, y) -> x /= y) . unequals

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c xs = [ x | x <- xs, x /= c ]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c xs = rmCharComp c xs == rmChar c xs

prop_rmChar' :: Char -> String -> Bool
prop_rmChar' c = all (/=c) . rmChar c



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = map toUpper . filter isAlpha

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (*2) . filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter (even . length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec []      = []
concatRec (x:xs)  = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] s     = s
rmCharsRec (x:xs) s = rmCharsRec xs (rmChar x s)

rmCharsFold :: String -> String -> String
rmCharsFold s xs = foldr rmChar xs s

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Vector = [Int]
type Matrix = [Vector]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform []      = True
uniform (x:xs)  = all (==x) xs

all' :: Eq a => (a -> Bool) -> [a] -> Bool
all' f = foldr (&&) True . map f

prop_uniform_pos :: Int -> Int -> Property
prop_uniform_pos n x = n > 0 ==> uniform xs && length (nub xs) == 1
  where xs = replicate n x

prop_uniform_neg :: Int -> Int -> Property
prop_uniform_neg x y = x /= y ==> not $ uniform [start..end]
  where start = min x y
        end   = max x y


-- b.
valid :: Matrix -> Bool
valid mat = validCols mat && validRows mat
  where
    validCols           = uniform . map length

    validRows ((_:_):_) = True
    validRows _         = False

-- 6.

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f x y | (x, y) <- zip xs ys ]

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) $ zip xs ys

-- 7.
plusM :: Matrix -> Matrix -> Maybe Matrix
plusM a b
  | valid a && valid b && sameSize  = Just addMat
  | otherwise                       = Nothing
    where
     addMat       = zipWith plusRow a b
     plusRow x y  = zipWith (+) x y

     sameSize     = map length a == map length b

-- 8.
dot :: Vector -> Vector -> Int
dot u v = sum $ zipWith (*) u v

shape :: Matrix -> (Int, Int)
shape a = (head (map length a), length a)

timesM :: Matrix -> Matrix -> Maybe Matrix
timesM a b
  | validShape  = Just c
  | otherwise   = Nothing
    where
      (_, m)  = shape a
      (m', _) = shape b

      validShape = m == m'

      b'  = transpose b
      c   = [ [ dot x y | y <- b' ] | x <- a ]


-- Optional material
-- 9.

type DVector = [Double]
type DMatrix = [DVector]


dot' :: DVector -> DVector -> Double
dot' u v = sum $ zipWith (*) u v

timesM' :: DMatrix -> DMatrix -> Maybe DMatrix
timesM' a b
  | validShape  = Just c
  | otherwise   = Nothing
    where
      (_, m)  = shape' a
      (m', _) = shape' b

      validShape = m == m'

      b'  = transpose b
      c   = [ [ dot' x y | y <- b' ] | x <- a ]


mat :: Int -> Int -> DMatrix
mat n m = [ [((fromIntegral i) * m')..((fromIntegral i) * m' + n')] | i <- [0..m] ]
  where m' = fromIntegral m
        n' = fromIntegral n

identity :: Int -> DMatrix
identity n = [ [ if i == j then 1.0 else 0.0 | i <- [0..n] ] | j <- [0..n] ]

extractSubMatrix :: Int -> DMatrix -> DMatrix
extractSubMatrix i = map del . tail
  where del xs = take i xs ++ drop (i + 1) xs

sub' :: Int -> Int -> DMatrix -> DMatrix
sub' col row = map (del col) . del row
  where del i xs = take i xs ++ drop (i + 1) xs

printMat :: DMatrix -> IO ()
printMat a = putStrLn $ intercalate "\n" $ map show a

shape' :: DMatrix -> (Int, Int)
shape' [] = (0,0)
shape' a  = (head (map length a), length a)

det :: DMatrix -> Double
det [[a,b],[c,d]] = a * d - b * c
det a             =
  let
    (n,_) = shape' a
    sub i = extractSubMatrix i a
    row   = head a
  in
    sum [ x_i * (-1) ^ i * det (sub i) | (x_i, i) <- zip row [0..n - 1] ]

minors :: DMatrix -> DMatrix
minors a = [ [ det (sub' j i a) | j <- [0..cols - 1]] | i <- [0..rows - 1] ]
  where (cols,rows) = shape' a

cofactors :: DMatrix -> DMatrix
cofactors a = [ [ (-1) ^ (i+j) * x_ij | (x_ij, j) <- zip row [0..cols - 1]] | (row, i) <- zip a [0..rows - 1] ]
  where
    (cols,rows) = shape' a

adjugate :: DMatrix -> DMatrix
adjugate a = transpose a

isSquare :: DMatrix -> Bool
isSquare a = n == p
  where (n,p) = shape' a

inverse :: DMatrix -> Maybe DMatrix
inverse a
  | d' == 0 || not (isSquare a) = Nothing
  | otherwise                   = Just (f a)
    where d'              = det a
          f [[a,b],[c,d]] = [[1/d' * d, 1/d' * (-b)], [1/d' * (-c), 1/d' * a]]
          f a'            = map (map (*(1 / d'))) $ adjugate $ cofactors $ minors a'

dVectorGen :: Int -> Gen DVector
dVectorGen n = vectorOf n arbitrary

dMatrixGen :: Int -> Gen DMatrix
dMatrixGen n = vectorOf n (dVectorGen n)

squareMatrixGen :: Gen DMatrix
squareMatrixGen = do
    size <- arbitrary `suchThat` (\x -> x > 1 && x < 8)
    dMatrixGen size

approxEqual :: Double -> Double -> Double -> Bool
approxEqual tol a b = abs (a - b) < tol

approxEqualMatrices :: Double -> DMatrix -> DMatrix -> Bool
approxEqualMatrices tol a b = and [ approxRow rowA rowB | (rowA, rowB) <- zip a b]
  where
    approxRow a' b' = and [ approxEqual tol valA valB | (valA, valB) <- zip a' b' ]

prop_inverse :: Property
prop_inverse = forAll squareMatrixGen $ \a ->
  let
    (n, _)  = shape' a
    Just a' = inverse a
    Just b  = timesM' a a'
    tol     = 1e-9
  in
    det a /= 0 ==> approxEqualMatrices tol b (identity n)
