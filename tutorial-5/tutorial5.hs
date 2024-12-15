-- Informatics 1 - Functional Programming 
-- Tutorial 5
--
-- Due: the tutorial of week 7 (2/3 November)

module Tutorial5 where


import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, Gen, listOf, forAll  )

-- Warmup exercises

-- The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [ Orange "Seville" 12
         , Apple "Granny Smith" False
         , Apple "Braeburn" True
         , Orange "Sanguinello" 10
         ]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange "Moro"        _)  = True
isBloodOrange (Orange "Sanguinello" _)  = True
isBloodOrange (Orange "Tarocco"     _)  = True
isBloodOrange _                         = False

-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments = sum . map segments . filter isBloodOrange

segments :: Fruit -> Int
segments (Orange _ n) = n
segments _            = 0

-- 3.
worms :: [Fruit] -> Int
worms = length . filter hasWorm

hasWorm :: Fruit -> Bool
hasWorm (Apple _ True)  = True
hasWorm _               = False

-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]

-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"


-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  eval e (Not p) || eval e q
eval e (p :<->: q)    =  (eval e p && eval e q) || (eval e (Not p) && eval e (Not q))

-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

-- 4.
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))
p3 = (Var "P" :&: ((Var "Q" :|: Var "R"))) :&: (Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R"))


-- 5. 
tautology :: Prop -> Bool
tautology p = and $ map (\e -> eval e p) $ envs $ names p

prop_taut1 :: Prop -> Bool
prop_taut1 p = tautology p || satisfiable (Not p)

prop_taut2 :: Prop -> Bool
prop_taut2 p = not (satisfiable p) || not (tautology (Not p))


-- 6.
p4 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")
p5 = (Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))
p6 = ((Var "P" :<->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))) :|: (Not (Var "P") :&: Var "Q")


-- 7.
equivalent :: Prop -> Prop -> Bool
equivalent p q = and [ eval e p == eval e q | e <- envs $ nub (names p ++ names q) ]

equivalent' :: Prop -> Prop -> Bool
equivalent' p q = tautology (p :<->: q)

equivalent'' :: Prop -> Prop -> Bool
equivalent'' p q = not $ satisfiable (Not (p :<->: q))

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent p q = equivalent p q == equivalent' p q && equivalent' p q == equivalent'' p q


-- 8.
subformulas :: Prop -> [Prop]
subformulas = nub . f
    where
        f (Var x)       = [Var x]
        f (F)           = [F]
        f (T)           = [T]
        f (Not p)       = Not p         : f p
        f (p :|: q)     = (p :|: q)     : f p ++ f q
        f (p :&: q)     = (p :&: q)     : f p ++ f q
        f (p :->: q)    = (p :->: q)    : f p ++ f q
        f (p :<->: q)   = (p :<->: q)   : f p ++ f q


-- Optional Material

-- 9.
-- check for negation normal form
isNNF :: Prop -> Bool
isNNF T             = True
isNNF F             = True
isNNF (Var x)       = True
isNNF (Not (Var x)) = True
isNNF (p :|: q)     = isNNF p && isNNF q
isNNF (p :&: q)     = isNNF p && isNNF q
isNNF _             = False

-- 10.
-- convert to negation normal form
toNNF :: Prop -> Prop
toNNF T                     = T
toNNF (Not T)               = F
toNNF F                     = F
toNNF (Not F)               = T
toNNF (Var x)               = Var x
toNNF (Not (Var x))         = Not (Var x)
toNNF (Not (Not (Var x)))   = Var x
toNNF (Not (Not p))         = toNNF p
toNNF (p :&: q)             = toNNF p       :&: toNNF q
toNNF (Not (p :&: q))       = toNNF (Not p) :|: toNNF (Not q)
toNNF (p :|: q)             = toNNF p       :|: toNNF q
toNNF (Not (p :|: q))       = toNNF (Not p) :&: toNNF (Not q)
toNNF (p :->: q)            = toNNF (Not p) :|: toNNF q
toNNF (Not (p :->: q))      = toNNF (Not ((Not p) :|: q))
toNNF (p :<->: q)           = toNNF ((p :->: q) :&: (q :->: p))
toNNF (Not (p :<->: q))     = toNNF (Not ((p :->: q) :&: (q :->: p)))

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p  =  isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p  =  equivalent p (toNNF p)


-- 11.
-- check whether a formula is in conj. normal form
isCNF :: Prop -> Bool
isCNF T                 = True
isCNF F                 = True
isCNF (Var _)           = True
isCNF (Not (Var _))     = True
isCNF (p :|: q)         = isClause (p :|: q)
isCNF (p :&: q)         = isCNF p && isCNF q
isCNF _                 = False

-- Helper function to check if a formula is a valid clause
isClause :: Prop -> Bool
isClause (Var _)        = True
isClause (Not (Var _))  = True
isClause (p :|: q)      = isClause p && isClause q
isClause _              = False

-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Prop]] -> Prop
listsToCNF = toConjunction . map toDisjunction

prop_listsToCNF :: [[Prop]] -> Bool
prop_listsToCNF = isCNF . listsToCNF

genVarLists :: Gen [[Prop]]
genVarLists = listOf (listOf genVar)

genVarList :: Gen [Prop]
genVarList = listOf genVar

genVar :: Gen Prop
genVar = liftM Var (elements ["P", "Q", "R", "S", "T"])

test_listsToCNF :: IO ()
test_listsToCNF = quickCheck (forAll genVarLists prop_listsToCNF)


toDisjunction :: [Prop] -> Prop
toDisjunction []     = F
toDisjunction [x]    = x
toDisjunction (x:xs) = x :|: toDisjunction xs

isDisjunction :: Prop -> Bool
isDisjunction T                 = True
isDisjunction F                 = True
isDisjunction (Var _)           = True
isDisjunction (Not (Var _))     = True
isDisjunction (Var _ :|: Var _) = True
isDisjunction (p :|: q)         = isDisjunction p && isDisjunction q
isDisjunction _                 = False

prop_isDisjunction :: [Prop] -> Bool
prop_isDisjunction = isDisjunction . toDisjunction

test_isDisjunction :: IO ()
test_isDisjunction = quickCheck (forAll genVarList prop_isDisjunction)

toConjunction :: [Prop] -> Prop
toConjunction []     = T
toConjunction [x]    = x
toConjunction (x:xs) = x :&: toConjunction xs


-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF T              = []
listsFromCNF F              = [[]]
listsFromCNF (Var x)        = [[Var x]]
listsFromCNF (Not (Var x))  = [[Not (Var x)]]
listsFromCNF (p :|: q)      = [disjunctionToList p ++ disjunctionToList q]
listsFromCNF (p :&: q)      = listsFromCNF p ++ listsFromCNF q

prop_listsFromCNF :: [[Prop]] -> Bool
prop_listsFromCNF p = listsFromCNF (listsToCNF p) == p

test_listsFromCNF :: IO ()
test_listsFromCNF = quickCheck (forAll genVarLists prop_listsFromCNF)

-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Prop -> [[Prop]]
toCNFList = simplifyCNFList . listsFromCNF . simplifyRec . toNNF

prop_toCNFList :: Prop -> Bool
prop_toCNFList p = equivalent (listsToCNF . toCNFList $ p) p

simplifyRec :: Prop -> Prop
simplifyRec p = f p (simplify p)
    where
        f prev new
            | prev == new = new
            | otherwise   = f new (simplify new)

---- Simplify function
simplify :: Prop -> Prop
simplify T                  = T
simplify F                  = F
simplify (Var x)            = Var x
simplify (Not (Var x))      = Not (Var x)
simplify (p :&: T)          = simplify p
simplify (T :&: p)          = simplify p
simplify (p :|: F)          = simplify p
simplify (F :|: p)          = simplify p
simplify (p :&: F)          = F
simplify (F :&: p)          = F
simplify (p :|: T)          = T
simplify (T :|: p)          = T
simplify (p :|: (q :&: r))  = simplify (p :|: q) :&: simplify (p :|: r)
simplify ((p :&: q) :|: r)  = simplify (p :|: r) :&: simplify (q :|: r)
simplify (p :&: q)          = simplify p :&: simplify q
simplify (p :|: q)          = simplify p :|: simplify q
simplify (Not p)            = Not (simplify p)

simplifyCNFList :: [[Prop]] -> [[Prop]]
simplifyCNFList = nub . map nub

prop_simplify :: Prop -> Bool
prop_simplify p = equivalent (simplify . toNNF $ p) p

disjunctionToList :: Prop -> [Prop]
disjunctionToList T                 = []
disjunctionToList F                 = []
disjunctionToList (Var x)           = [Var x]
disjunctionToList (Not (Var x))     = [Not (Var x)]
disjunctionToList (p :|: q)         = disjunctionToList p ++ disjunctionToList q
disjunctionToList (p :&: q)         = [p :&: q]
disjunctionToList other             = [other]

-- convert to conjunctive normal form
toCNF :: Prop -> Prop
toCNF p  =  listsToCNF (toCNFList p)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Prop -> Bool
prop_CNF p  =  equivalent p (toCNF p)


-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
