-- Informatics 1 - Functional Programming 
-- Tutorial 6
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

module Tutorial6 where

import LSystem
import Test.QuickCheck
import Debug.Trace (trace)

-- On debian, please run the following in order to use glut:
-- apt update && apt install freeglut3-dev -y
-- cabal update
-- cabal install --lib GLUT
--
-- to make life easier, please create .ghci file with the following contents:
-- :set -package OpenGL
-- :set -package random
-- :l tutorial6.hs


-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit         = []
split (Go n)      = [Go n]
split (Turn a)    = [Turn a]
split (GrabPen p) = [GrabPen p]
split (p :#: q)   = split p ++ split q

-- 1b. join
join :: [Command] -> Command
join []     = Sit
join (x:xs) = f x xs
  where
    f c []      = c
    f c (y:ys)  = f (c :#: y) ys

-- 1c. equivalent
equivalent :: Command -> Command -> Bool
equivalent p q = split p == split q

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c

prop_split :: Command -> Bool
prop_split = all f . split
  where
    f (_ :#: _) = False
    f Sit       = False
    f _         = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n c = f (n - 1) c
  where
    f 0 c' = c'
    f m c' = f (m - 1) (c' :#: c)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d i = copy i (Go d :#: Turn (360.0 / fromIntegral i))


-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _    0 _    _      = Sit
spiral side n step angle
  | side <= 0 = Sit
  | otherwise = Go side :#: Turn angle :#: spiral (side + step) (n - 1) step angle


-- Exercise 4
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise = join . collapse . filter f . split
  where
    f Sit      = False
    f (Turn 0) = False
    f (Go 0)   = False
    f _        = True

collapse :: [Command] -> [Command]
collapse c = let
    c' = f c
  in
    if (c == c') then c' else collapse c'
  where
    f [] = []
    f (Turn d : Turn e  : rest) = if (d + e == 0) then f rest else f (Turn (d + e) : rest)
    f (Go d   : Go e    : rest) = if (d + e == 0) then f rest else f (Go (d + e) : rest)
    f (p : rest)                = p : collapse rest


isOptimal :: Command -> Bool
isOptimal c = any f c' || g c'
  where
    c' = split c

    f Sit      = True
    f (Turn 0) = True
    f (Go 0)   = True
    f _        = False

    g [] = True
    g (Turn _ : Turn _  : _ ) = False
    g (Go _   : Go _    : _ ) = False
    g (_ : rest )             = g rest

prop_optimise :: Command -> Bool
prop_optimise = isOptimal . optimise

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = f
    where
      f 0 = Go 10
      f x = g (x - 1) :#: p :#: f (x - 1) :#: p :#: g (x - 1)
      g 0 = Go 10
      g x = f (x - 1) :#: n :#: g (x - 1) :#: n :#: f (x - 1)
      n   = Turn   60
      p   = Turn (-60)


-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
  where
    f 0 = Go 10
    f x = f (x - 1) :#: p :#: f (x - 1) :#: n :#: n :#: f (x - 1) :#: p :#: f (x - 1)
    n   = Turn   60
    p   = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert = l
  where
    f   = Go 10
    l 0 = Sit
    l x = p :#: r (x - 1) :#: f :#: n :#: l (x - 1) :#: f :#: l (x - 1) :#: n :#: f :#: r (x - 1) :#: p
    r 0 = Sit
    r x = n :#: l (x - 1) :#: f :#: p :#: r (x - 1) :#: f :#: r (x - 1) :#: p :#: f :#: l (x - 1) :#: n
    n   = Turn 90
    p   = Turn (-90)

kochAntisnowflake :: Int -> Command
kochAntisnowflake x = f x :#: p :#: p :#: f x :#: p :#: p :#: f x
  where
    f 0 = Go 10
    f x = f (x - 1) :#: p :#: f (x - 1) :#: n :#: n :#: f (x - 1) :#: p :#: f (x - 1)
    n   = Turn 60
    p   = Turn (-60)

-- Bonus L-Systems

peanoGosper :: Int -> Command
peanoGosper = f
  where
    f 0 = Go 10
    f x = f ( x - 1) :#: p :#: g (x - 1) :#: p :#: p :#: g (x - 1) :#: n :#: f ( x - 1) :#: n :#: n :#: f ( x - 1) :#: f ( x - 1) :#: n :#: g (x - 1) :#: p
    g 0 = Go 10
    g x = n :#: f (x - 1) :#: p :#: g (x - 1) :#: g (x - 1) :#: p :#: p :#: g (x - 1) :#: p :#: f (x - 1) :#: n :#: n :#: f (x - 1) :#: n :#: g (x - 1)
    n   = Turn 60
    p   = Turn (-60)


cross :: Int -> Command
cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x :#: n
  where
    f 0 = Go 10
    f x = f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1)
    n   = Turn 90
    p   = Turn (-90)

branch :: Int -> Command
branch = g
  where
    f 0 = Go 10
    f x = f (x - 1) :#: f (x - 1)
    g 0 = Go 10
    g x = f (x - 1) :#: n :#: Branch (Branch (g (x - 1)) :#: p :#: g (x - 1)) :#: p :#: f (x - 1) :#: Branch (p :#: f (x - 1) :#: g (x - 1)) :#: n :#: g (x - 1)
    n   = Turn   22.5
    p   = Turn (-22.5)


thirtytwo :: Int -> Command
thirtytwo x = f x :#: p :#: f x :#: p :#: f x :#: p :#: f x
  where
    f 0 = Go 10
    f x = n :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1) :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: p
    n   = Turn 90
    p   = Turn (-90)


binaryTree :: Int -> Command
binaryTree = f
  where
    f 0 = Go 1
    f x = g (x - 1) :#: Branch (p :#: f (x - 1)) :#: Branch (n :#: f (x - 1))
    g 0 = Go 1
    g x = g (x - 1) :#: g (x - 1)
    p   = Turn   45
    n   = Turn (-45)

dragonCurve :: Int -> Command
dragonCurve = f
  where
    f 0 = Go 10
    f x = f (x - 1) :#: p :#: g (x - 1)
    g 0 = Go 10
    g x = f (x - 1) :#: n :#: g (x - 1)

    p   = Turn   90
    n   = Turn (-90)

barnsleyFern :: Int -> Command
barnsleyFern = x
  where
    x 0 = Sit
    x n = f (n - 1) :#: Branch (p :#: x (n - 1)) :#: f (n - 1) :#: Branch (n' :#: x (n - 1)) :#: p :#: x (n - 1)

    f 0 = Go 10
    f n = f (n - 1) :#: f (n - 1)

    p  = Turn   25
    n' = Turn (-25)

levyCurve :: Int -> Command
levyCurve = f
  where
    f 0 = Go 10
    f x = p :#: f (x - 1) :#: n :#: n :#: f (x - 1) :#: p

    p   = Turn   45
    n   = Turn (-45)
