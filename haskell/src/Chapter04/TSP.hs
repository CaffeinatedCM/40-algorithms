{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, BangPatterns #-}

module Chapter04.TSP where

import Data.List.Extra
import System.Random

data City a where
    City :: (Floating a) => a -> a -> City a

instance Show a => Show (City a) where
    show (City x y) = "City at (" ++ show x ++ ", " ++ show y ++ ")"

instance Eq a => Eq (City a) where
    (==) (City x1 y1) (City x2 y2) = x1 == x2 && y1 == y2

tourDistance :: Floating a => [City a] -> a
tourDistance []       = 0
tourDistance [x]      = 0
tourDistance (x:y:xs) = cityDistance x y + rest
                        where !rest = tourDistance (y:xs)

cityDistance :: Floating a => City a -> City a -> a
cityDistance (City x1 y1) (City x2 y2) = sqrt $ (x2 - x1)**2 + (y2-y1)**2

randomCity :: StdGen -> (City Float, StdGen)
randomCity seed = let (x, s) = random seed
                      (y, s2) = random s
                  in (City (x*100) (y*100), s2)

randomCities :: Int -> StdGen -> [City Float]
randomCities n = take n . unfoldr (Just . randomCity)

randomCitiesIO :: Int -> IO [City Float]
randomCitiesIO n = do randomCities n <$> newStdGen

compareTours :: (Floating a, Ord a) => [City a] -> [City a] -> Ordering
compareTours x y = compare (tourDistance x) (tourDistance y)

-- Brute Force 
bruteForce :: (Floating a, Ord a) => [City a] -> [City a]
bruteForce c = minimumBy compareTours $ permutations c

-- Greedy
greedy :: (Floating a, Ord a) => [City a] -> [City a]
greedy []     = []
greedy [x]    = [x]
greedy (x:xs) = x : greedy (neighbor:rest)
                where (neighbor, rest) = nearestNeighbor x xs

nearestNeighbor :: (Floating a, Ord a) => City a -> [City a] -> (City a, [City a])
nearestNeighbor c cs = (neighbor, delete neighbor cs)
                       where neighbor = minimumOn (cityDistance c) cs