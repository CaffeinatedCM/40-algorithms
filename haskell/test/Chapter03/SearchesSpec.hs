module Chapter03.SearchesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.List (sort)

import Chapter03.Searches 

tests :: TestTree
tests = testGroup "Searches" [ testProperty "Linear Search" linearSearchTest
                             , testProperty "Binary Search" binarySearchTest
                             , testProperty "Interpolation Search" interpolationSearchTest]

linearSearchTest :: [Int] -> Int -> Property
linearSearchTest xs x =
    checkCoverage $
    cover 10 (x `elem` xs) "Elem is included" $
    linearSearch xs x == (x `elem` xs)


binarySearchTest :: [Int] -> Int -> Property
binarySearchTest xs x =
    checkCoverage $
    cover 10 (x `elem` xs) "Elem is included" $
    binarySearch (sort xs) x == (x `elem` xs)

interpolationSearchTest :: [Int] -> Int -> Property
interpolationSearchTest xs x =
    checkCoverage $
    cover 10 (x `elem` xs) "Elem is included" $
    interpolationSearch (sort xs) x == (x `elem` xs)
