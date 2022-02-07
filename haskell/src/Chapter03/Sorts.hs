
module Chapter03.Sorts where

import Data.List (transpose, unfoldr)

-- Bubble Sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = bubbleSortOuter (length xs) xs

bubbleSortOuter :: Ord a => Int -> [a] -> [a]
bubbleSortOuter 0    xs = xs
bubbleSortOuter lIdx xs = bubbleSortOuter (lIdx-1) $ bubbleSortInner xs 0 lIdx

bubbleSortInner :: Ord a => [a] -> Int -> Int -> [a]
bubbleSortInner [x]  _   _                      = [x]
bubbleSortInner (x:y:xs) idx lIdx | idx >= lIdx = x : y : xs
bubbleSortInner (x:y:xs) idx lIdx | idx <  lIdx = min x y : bubbleSortInner (max x y : xs) (idx+1) lIdx

-- Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl (flip insert) [] -- Build a new list by inserting each element from the input list

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x >= y    = y:insert x ys
                | otherwise = x:y:ys

-- Merge Sort
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
                where (left, right) = splitAt (length xs `div` 2) xs


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Shell Sort
shellSort :: Ord a => [a] -> [a] -- Run a phase for each distance starting from half the list to no gap
shellSort xs = foldr shellSortOuter xs gaps
    where gaps = takeWhile (> 0) [length xs `quot` n | n <- [2,4..]] 

shellSplit :: Int -> [a] -> [[a]] -- Split the list at a distance of d
shellSplit d = transpose . takeWhile (not . null) . unfoldr (Just . splitAt d)

shellCombine :: [[a]] -> [a] -- Re-combine the list
shellCombine = concat . transpose

shellSortOuter :: (Ord a) => Int -> [a] -> [a] -- Phase, split the list into sublists (distance of d), sort them with insertion sort, then merge back to one list
shellSortOuter d = shellCombine . map insertionSort . shellSplit d

-- Selection Sort
-- Each pass of the sort finds the largest value and moves it to the end of the list
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = selectionSort (left ++ drop 1 right) ++ [m]
    where (mIdx, m)     = findMax xs 0 (0, head xs)
          (left, right) = splitAt mIdx xs

findMax :: Ord a => [a] -> Int -> (Int, a) -> (Int, a)
findMax [] _ (mIdx, m)                   = (mIdx, m)
findMax (x:xs) idx (mIdx, m) | x > m     = findMax xs (idx+1) (idx, x)
                             | otherwise = findMax xs (idx+1) (mIdx, m)