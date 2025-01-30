-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 10 - due: 23--24 November

module Tutorial8 where

import Data.List
import Test.QuickCheck
import Data.Char
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set


-- Type declarations

type FSM q        = ([q], Alphabet, q, [q], [Transition q])
type Alphabet     = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (u,_,_,_,_) = u
alph   (_,a,_,_,_) = a
start  (_,_,s,_,_) = s
final  (_,_,_,f,_) = f
trans  (_,_,_,_,t) = t


-- 2.
delta :: (Eq q) => FSM q -> Char -> q -> [q]
delta fsm symbol state = [ a' | (a,x,a') <- trans fsm, a == state, x == symbol ]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts fsm xs = f [start fsm] xs
  where
    finalQ    = final fsm

    f [] _            = False
    f [state] (y:ys)  = f (delta fsm y state) ys
    f states []       = any (\state -> state `elem` finalQ) states
    f (x:xs) string   = f [x] string || f xs string



-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = sort . nub


-- 5.

ddelta :: (Ord q) => FSM q -> Char -> [q] -> [q]
ddelta fsm symbol = canonical . concat . map (delta fsm symbol)

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm states = canonical $ states ++ [ ddelta fsm symbol state | state <- states, symbol <- alph fsm ]


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm q = let
    q' = next fsm q
  in
    if (q == q') then q' else reachable fsm q'


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm qs = [ q' | q <- final fsm, q' <- qs, q `elem` q' ]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm qs = [ (q, c, ddelta fsm c q) | q <- qs, c <- alph fsm ]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic fsm = let
    u = reachable fsm [[start fsm]]
    a = alph fsm
    s = [start fsm]
    f = dfinal fsm u
    t = dtrans fsm u
  in
    (u,a,s,f,t)

-- Optional Material
--11.
charFSM :: Char -> FSM Int
charFSM c = let
    u = [0, 1]
    a = [c]
    s = 0
    f = [1]
    t = [(0,c,1)]
  in
    (u,a,s,f,t)

emptyFSM :: FSM Int
emptyFSM = let
    u = [0,1]
    a = []
    s = 0
    f = [1]
    t = []
   in
     (u,a,s,f,t)

--12
intFSM :: (Ord q) => FSM q -> FSM Int
intFSM fsm = let
    os = states fsm

    st = 0
    ns = [st..length os + st]

    s' = zip (states fsm) ns

    u       = ns
    a       = alph fsm
    Just s  = lookup (start fsm) s'
    f       = map snd $ filter (\(oldS, newS) -> oldS `elem` final fsm) s'
    t       = [ (fromMaybe (-1) $ lookup q s', x, fromMaybe (-1) $ lookup q' s') | (q, x, q') <- trans fsm ]
  in
    (u,a,s,f,t)

concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM p q = let
    a' = intFSM p
    b' = shiftStateSpace (length (states a')) (intFSM q)

    u = states a' ++ states b'
    a = alph a' ++ alph b'
    s = start a'
    f = final b'
    t = trans a' ++ trans b'
  in
    (u,a,s,f,t)

shiftStateSpace :: Int -> FSM Int -> FSM Int
shiftStateSpace n fsm = let
    u = map (+n) (states fsm)
    a = alph fsm
    s = (start fsm) + n
    f = [ q + n |  q <- final fsm ]
    t = [ (q + n, x, q' + n) | (q,x,q') <- trans fsm ]
  in
    (u,a,s,f,t)


--13
stringFSM :: String -> FSM Int
stringFSM str = let
    s' = 0
    f' = length str

    u = s' : [(s' + 1)..f']
    a = nub str
    s = s'
    f = [f']
    t = [ (q, x, q + 1) | (x, q) <- zip str [s'..length str] ]
  in
    (u,a,s,f,t)




-- For quickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_stringFSM1 n = accepts (stringFSM n') n'
      where n' = safeString n
prop_stringFSM2 n m = (m' == n') || (not $ accepts (stringFSM n') m')
                where m' = safeString m
                      n' = safeString n

--14
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM fsm = let
    tMap  = transitionMap fsm
    tDiff = alphSetDifference (Set.fromList $ alph fsm) tMap
    tMiss = missingTransitions tDiff

    trapped = [ (Nothing,x,Nothing) | x <- alph fsm ]

    t = [ (Just q, x, Just q') | (q, x, q') <- trans fsm ]

    u   = Nothing : map Just (states fsm)
    a   = alph fsm
    s   = Just (start fsm)
    f   = map Just (final fsm)
    t'  = trapped ++ tMiss ++ t
  in
    (u,a,s,f,t')


emptyTransitionMap :: Ord q => [q] -> Map.Map q (Set.Set Char)
emptyTransitionMap = foldr (\x acc -> Map.insert x Set.empty acc ) Map.empty

transitionMap :: Ord q => FSM q -> Map.Map q (Set.Set Char)
transitionMap fsm = let
    init        = emptyTransitionMap (states fsm)
    f (q,x,_) m = addToSet q x m
  in
    foldr f init (trans fsm)


addToSet :: (Ord k, Ord v) => k -> v -> Map.Map k (Set.Set v) -> Map.Map k (Set.Set v)
addToSet key value = Map.alter f key
  where
    f Nothing   = Just (Set.singleton value)
    f (Just s)  = Just (Set.insert value s)

alphSetDifference :: (Ord k, Ord v) => Set.Set v -> Map.Map k (Set.Set v) -> Map.Map k (Set.Set v)
alphSetDifference alphabet = Map.map f
  where
    f s = Set.difference alphabet s

missingTransitions :: (Ord k, Ord v) => Map.Map k (Set.Set v) -> [(Maybe k, v, Maybe k)]
missingTransitions = concat . Map.elems . Map.mapWithKey f . Map.filter (not . Set.null)
  where
    f k sy = [ (Just k, s, Nothing) | s <- Set.toList sy ]

unionFSM :: (Ord q) => FSM q -> FSM q -> FSM Int
unionFSM x y = let
    u = [ (q, q') | q <- states x, q' <- states y ]
    a = nub (alph x ++ alph y)
    s = (start x, start y)
    f = [ (q, q') | (q, q') <- u, q `elem` (final x) || q' `elem` (final y) ]
    t = transProduct x y a
  in
    intFSM $ completeFSM (u,a,s,f,t)

transProduct :: Eq a => FSM a -> FSM a -> Alphabet -> [Transition (a,a)]
transProduct a b = concatMap f
  where
    f symbol = let
        a' = [ (q,x,q') | (q,x,q') <- trans a, x == symbol ]
        b' = [ (q,x,q') | (q,x,q') <- trans b, x == symbol ]
      in
        [ ((qA,qB),symbol,(qA',qB')) | (qA,_,qA') <- a', (qB,_,qB') <- b' ]

prop_union n m l =  accepts (unionFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l'|| accepts (stringFSM m') l') &&
                    accepts (unionFSM (stringFSM n') (stringFSM m')) n' && accepts (unionFSM (stringFSM n') (stringFSM m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

--15
star :: (Ord q) => FSM q -> FSM q
star fsm = let
    u = states fsm
    a = alph fsm
    s = start fsm
    f = [start fsm]
    t = [ (q,x,  if (q' `elem` final fsm) then s else q') | (q,x,q') <- trans fsm ]
  in
    (u,a,s,f,t)

    
prop_star a n = (star $ stringFSM a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ stringFSM a') `accepts` ""
      where a' = safeString a

--16
complement :: (Ord q) => FSM q -> FSM Int
complement b =
  let
    b' = intFSM (completeFSM b)
    u  = states b'
    a  = alph b'
    s' = start b'
    f' = u \\ final b'
    t  = trans b'
  in
    (u, a, s', f', t)

prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ stringFSM n') m'
                      && (not $ accepts (complement $ stringFSM n') n)
                      where n' = safeString n
                            m' = safeString m

-- 17.
intersectFSM :: (Ord q1, Ord q2)
             => FSM q1
             -> FSM q2
             -> FSM (q1, q2)
intersectFSM (qsA, alphaA, startA, finalsA, transA)
             (qsB, alphaB, startB, finalsB, transB) =
  let
    alpha       = nub (alphaA ++ alphaB)
    states      = [ (qA, qB) | qA <- qsA, qB <- qsB ]
    start       = (startA, startB)
    finals      = [ (qA, qB) | qA <- finalsA, qB <- finalsB ]
    transitions =
      [ ((qA, qB), sym, (qA', qB'))
      | (qA,  sym,  qA') <- transA
      , (qB,  sym', qB') <- transB
      , sym == sym'
      ]

  in
    ( states
    , alpha
    , start
    , finals
    , transitions
    )
                
prop_intersect n m l = accepts (intersectFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l' && accepts (stringFSM m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l



prop1 a b = star ((stringFSM a') `unionFSM` (stringFSM b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((stringFSM a') `intersectFSM` (intFSM ((stringFSM b') `unionFSM` (stringFSM a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b


