-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT,
                    testTree
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf                   = 0
size (Node _ _ left right)  = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf                  = 0
depth (Node _ _ left right) = max (1 + depth left) (1 + depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf                   = []
toList (Node k v left right)  = toList left ++ [(k, v)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf                              = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get _ Leaf                    = Nothing
get key (Node k v left right)
  | key == k  = Just v
  | key <= k  = get key left
  | otherwise = get key right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (uncurry set) Leaf


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

reverseT :: Ord k => Keymap k a -> Keymap k a
reverseT Leaf                  = Leaf
reverseT (Node k v left right) = Node k v right left

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT key (Node k v left right)
  | k == key  = left
  | k > key   = filterLT key left
  | k < key   = Node k v left (filterLT key right)
  | otherwise = Leaf

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT key (Node k v left right)
  | k == key  = right
  | k < key   = filterGT key right
  | k > key   = Node k v (filterGT key left) right
  | otherwise = Leaf

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf b = b
merge a Leaf = a
merge (Node k v left right) b = let
    lt = filterLT k b
    gt = filterGT k b
  in
    Node k v (merge left lt) (merge right gt)

prop_mergeSize :: (Ord k, Eq a) => Keymap k a -> Keymap k a -> Bool
prop_mergeSize a b = size (merge a b) == length (nub (toList a ++ toList b))

prop_merge :: (Ord k, Eq a) => Keymap k a -> Keymap k a -> Bool
prop_merge a b = let
    merged = merge a b

    allFromA = all (\(k, _) -> hasElement k merged) (toList a)
    allFromB = all (\(k, _) -> hasElement k merged) (toList b)
  in
    allFromA && allFromB && isSorted merged

hasElement :: Ord k => k -> Keymap k a -> Bool
hasElement key t = case get key t of
  Nothing -> False
  Just _  -> True

intTree :: Int -> Int -> Keymap Int Int
intTree start end = fromList [ (x, x * 10) | x <- [start..end] ]

prop_isSorted :: Keymap Int Int -> Bool
prop_isSorted = isSorted

isSorted :: Ord k => Keymap k v -> Bool
isSorted Leaf                   = True
isSorted (Node k _ left right)  = f k (<) left && f k (>) right
  where
    f _ _ Leaf                = True
    f k pred (Node k' _ l r) = pred k' k && f k' (<) l && f k' (>) r

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del _ Leaf                    = Leaf
del key (Node k v left right)
  | key == k  = merge left right
  | otherwise = Node k v (del key left) (del key right)

prop_del :: Int -> Int -> Keymap Int Int -> Bool
prop_del k v t = not $ hasElement k $ del k $ set k v t

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f Leaf                   = Leaf
select f (Node k v left right)
  | f v       = Node k v (select f left) (select f right)
  | otherwise = select f (merge left right)

prop_select :: Int -> Keymap Int Int -> Bool
prop_select p = allT (>p) . select (>p)

allT :: Ord k => (a -> Bool) -> Keymap k a -> Bool
allT _ Leaf                  = True
allT f (Node _ v left right) = f v && allT f left && allT f right

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary