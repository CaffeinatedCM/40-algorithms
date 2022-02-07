module Chapter03.SortsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.List (sort)

import Chapter03.Sorts 

tests :: TestTree
tests = testGroup "Sorts" [ testProperty "Bubble Sort" bubbleSortTest
                          , testProperty "Insertion Sort" insertionSortTest 
                          , testProperty "Merge Sort" mergeSortTest
                          , testProperty "Shell Sort" shellSortTest
                          , testProperty "Selection Sort" selectionSortTest]

bubbleSortTest :: [Int] -> Bool
bubbleSortTest xs = bubbleSort xs == sort xs

insertionSortTest :: [Int] -> Bool
insertionSortTest xs = insertionSort xs == sort xs

mergeSortTest :: [Int] -> Bool
mergeSortTest xs = mergeSort xs == sort xs

shellSortTest :: [Int] -> Bool
shellSortTest xs = shellSort xs == sort xs

selectionSortTest :: [Int] -> Bool
selectionSortTest xs = selectionSort xs == sort xs