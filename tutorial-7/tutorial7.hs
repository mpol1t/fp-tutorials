-- Informatics 1 Functional Programming
-- Tutorial 7
--
-- Due: 17/18 November

module Tutorial7 where

import System.Random

-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen = maximum . map (length . fst . snd)

formatLine :: Int -> (Barcode, Item) -> String
formatLine m (code, (product, unit)) = code ++ "..." ++ rightPad m '.' product ++ "..." ++ unit

rightPad :: Int -> Char -> String -> String
rightPad n char str = str ++ replicate (n - length str) char

showCatalogue :: Catalogue -> String
showCatalogue c = let
    c'      = toList c
    maxLen  = longestProductLen c'
  in
    concat $ map (\x -> (formatLine maxLen x) ++ "\n") c'
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing   = []
maybeToList (Just x)  = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [ x | Just x <- xs ]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems codes cat = catMaybes $ map (\code -> get code cat) codes






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
