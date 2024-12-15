-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (26/27 Oct)

module Tutorial4
    ( sameString
    , prefix
    , contains
    , takeUntil
    , dropUntil
    , split
    , reconstruct
    , extract
    , extractAll
    , linksFromHTML
    , takeEmails
    , link2pair
    , emailsFromHTML
    , findEmail
    , emailsByNameFromHTML
    , hasInitials
    , emailsByMatchFromHTML
    , emailsByInitialsFromHTML
    , emailsByMyCriteriaFromHTML
    , ppAddrBook
    , testURL
    , getURL
    , Link
    , Name
    , Email
    ) where

import Data.List (nub, intercalate)
import Data.Maybe (fromMaybe)
import Data.Char
import Test.QuickCheck
import Network.HTTP.Conduit (simpleHttp)
import Fixtures
import qualified Data.ByteString.Lazy.Char8 as L8
import Types

getURL :: String -> IO String
getURL url = do
    response <- simpleHttp url
    return $ L8.unpack response

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML name html)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
toLowerCase :: String -> String
toLowerCase = map toLower

sameString :: String -> String -> Bool
sameString a b = toLowerCase a == toLowerCase b

-- 2.
prefix :: String -> String -> Bool
prefix a b = sameString a (take (length a) b)

-- 3.
contains :: String -> String -> Bool
contains a b = any (prefix b) (tails a)
  where tails xs = [drop i xs | i <- [0..length xs]]

-- 4.
takeUntil :: String -> String -> (String, String)
takeUntil _ [] = ([], [])
takeUntil p s
  | prefix p s = ([], drop (length p) s)
  | otherwise  = let (h, t) = takeUntil p (tail s) in (head s : h, t)

dropUntil :: String -> String -> String
dropUntil p s = snd (takeUntil p s)

-- 5.
split :: String -> String -> Maybe [String]
split [] _  = Nothing
split sep s = Just $ splitHelper sep s
  where
    splitHelper _ [] = []
    splitHelper sep s =
      let
        (chunk, rest) = takeUntil sep s
      in
        chunk : if contains rest sep then splitHelper sep rest else [rest]

reconstruct :: String -> [String] -> String
reconstruct _ [] = ""
reconstruct sep xs = foldr1 (\x acc -> x ++ sep ++ acc) xs

extract :: String -> String -> String -> (String,String)
extract prefix postfix s = takeUntil postfix (dropUntil prefix s)

extractAll :: String -> String -> String -> [String]
extractAll _ _ [] = []
extractAll prefix postfix s =
    let
        (w, rest) = extract prefix postfix s
    in
        case rest of
            []  -> []
            _   -> w : extractAll prefix postfix rest

linkPrefix :: String
linkPrefix = "<a href=\""

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML = tail . fromMaybe [] . split linkPrefix

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails = filter (prefix "mailto")

-- 8.
link2pair :: Link -> Maybe (Name, Email)
link2pair l = do
    (email, rest) <- safeTakeUntil "\">" l
    (name, _)     <- safeTakeUntil "</a>" rest
    return (name, dropUntil "mailto:" email)

safeTakeUntil :: String -> String -> Maybe (String, String)
safeTakeUntil p s =
    let
        (a, b) = takeUntil p s
    in
        if null a && null b then Nothing else Just (a, b)

-- 9.
emailsFromHTML :: HTML -> [(Name, Email)]
emailsFromHTML html =
    nub [pair | Just pair <- map link2pair (takeEmails (linksFromHTML html))]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook

-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail n = filter (\(name, _) -> contains name n)

-- 11.
emailsByNameFromHTML :: Name -> HTML -> [(Name,Email)]
emailsByNameFromHTML name = findEmail name . emailsFromHTML


-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials initials name =
    case split " " name of
        Just parts  -> map head parts == initials
        Nothing     -> False

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f = filter (f . fst) . emailsFromHTML

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML initials = emailsByMatchFromHTML (hasInitials initials)

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria name = reverse name == name

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML = emailsByMatchFromHTML myCriteria

-- 15
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr =
    let
        nameWidth   = maximum $ map (length . fst) addr
        justify n s = s ++ replicate (n - length s) ' '
    in
        unlines [justify nameWidth name ++ "   " ++ email | (name, email) <- addr]