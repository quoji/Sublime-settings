-- Assignment 2, CSCE-314
-- Section: 502	 
-- Student Name: Stephen Brownlee
-- UIN: 224005487
-- learnyouahaskell.com 

module Main where

import Test.HUnit
import System.Exit

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) 

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

mySum :: [Int] -> Int
mySum [] = 0 
mySum (x:xs) = x + mySum xs 

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) =  x ++ flatten xs

myLength :: [a] -> Int
myLength [] = 0 
myLength (_:xs) =  1 + myLength xs 

isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement a (x:xs)
    | a == x  = True
    | otherwise = a `isElement` xs 

type Set a = [a]

mkSet :: Eq a => [a] -> Set a
mkSet [] = []
mkSet (x:xs) 
    | x `isElement` xs = mkSet xs
    | otherwise = [x] ++ mkSet xs 

size ::  Set a -> Int
size [] = 1
size a = myLength a

subset :: Eq a => Set a -> Set a -> Bool 
subset [] _ = True
subset (x:xs) a
    | x `isElement` a = subset xs a
    | otherwise = False

setEqual :: Eq a => Set a -> Set a -> Bool
setEqual [] [] = True
setEqual _ [] = False
setEqual [] _ = False
setEqual (x:xs) (y:ys)  
    | x `isElement` (y:ys) && y `isElement` (x:xs) = not (subset xs ys)
    | otherwise = False

powerset :: Set a -> Set (Set a)
powerset [] = [] 
powerset (x:xs) =  [x:xs] ++ [[x]] ++ powerset xs

myTestList = 
  TestList [ 
    "factorial" ~: factorial 3 ~=? 6
    
    , "fibonacci" ~: fibonacci 4 ~=? 3

    , "mySum" ~: mySum [1..10] ~=? 55
    
    , "flatten 1" ~: flatten [[]::[Int]] ~=? []
    , "flatten 2" ~: flatten [[]::[Int], [], []] ~=? []
    , "flatten 3" ~: flatten [[1], [2, 3, 4], [], [5, 6]] ~=? [1, 2, 3, 4, 5, 6]
      
    , "myLength" ~: myLength [1, 2, 3] ~=? 3
      
    , "isElement 1" ~: (isElement 'c' "abcd") ~=? True
    , "isElement 2" ~: (isElement 'e' "abcd") ~=? False
      
    , "mkSet 1" ~: size (mkSet "abcdaab") ~=? 4
    , "mkSet 2" ~: size (mkSet "abc") ~=? 3
      
    , "subset 1" ~: subset (mkSet "dbdb") (mkSet "abcdaab") ~=? True
    , "subset 2" ~: subset (mkSet "abc") (mkSet "bcccbhk") ~=? False
      
    , "setEqual 1" ~: setEqual (mkSet "abc") (mkSet "abcf") ~=? False
    , "setEqual 2" ~: setEqual (mkSet "abccbac") (mkSet "cbabc") ~=? True
      
    , "powerset 1" ~: size (powerset (mkSet []::[Int])) ~=? 1
    , "powerset 2" ~: size (powerset (mkSet [1])) ~=? 2
    , "powerset 3" ~: size (powerset (mkSet [1, 2])) ~=? 4
    ]

main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          exitWith (codeGet errs fails)
          
codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess