module Chapter3.Searches where

-- Linear Search
linearSearch :: Eq a => [a] -> a -> Bool
linearSearch [] _ = False
linearSearch (x:xs) i | x == i    = True
                      | otherwise = linearSearch xs i 

-- Binary Search
binarySearch :: Ord a => [a] -> a -> Bool
binarySearch [] _ = False
binarySearch xs i = case compare i (xs !! midpoint) of
                        EQ -> True
                        LT -> binarySearch leftPart i
                        GT -> binarySearch (drop 1 rightPart) i
                    where midpoint = length xs `quot` 2
                          (leftPart, rightPart) = splitAt midpoint xs

-- Interpolation Search
interpolationSearch :: (Ord a, Num a, Integral a) => [a] -> a -> Bool
interpolationSearch [] _ = False
interpolationSearch xs i    | head xs == i = True
                            | last xs /= head xs && i >= head xs && i <= last xs  = 
                                        case compare i (xs !! midpoint) of
                                            EQ -> True
                                            LT -> interpolationSearch leftPart i
                                            GT -> interpolationSearch (drop 1 rightPart) i
                                        where midpoint = fromIntegral $ fromIntegral (length xs - 1) `div` (last xs - head xs) * (i - head xs)
                                              (leftPart, rightPart) = splitAt midpoint xs
interpolationSearch _ _ | otherwise = False