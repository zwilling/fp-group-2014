-- authors: Frederik Zwilling (304314) and Roman Feldhoff (307750)

import Data.List

-- 3a
isEven :: Int -> Bool
isEven 0 = True
isEven x | x > 0 = not (isEven (x-1))
         | x < 0 = not (isEven (x+1))

-- 3b
countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs) | isEven x = 1 + countEven xs
                 | otherwise = countEven xs
                            
-- 3c                              
containsAll :: [Int] -> [Int] -> Bool
containsAll [] ys = True
containsAll xs [] = False
containsAll xs (y:ys) = containsAll (delete y xs) ys

-- 3d
prefixsum :: [Int] -> [Int]
prefixsum [] = []
prefixsum [x] = [x]
prefixsum (x1:x2:xs) = x1 : prefixsum ((x1+x2):xs)

-- 4
deleteAll :: Int -> [Int] -> [Int]
deleteAll x [] = []
deleteAll x (y:ys) | elem x [y] = deleteAll x ys
                   | otherwise = y:(deleteAll x ys)

removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:(removeDuplicates (deleteAll x xs))

(+|+) :: [Int] -> [Int] -> [Int]
(+|+) xs ys = removeDuplicates (xs ++ ys)
infixr 6 +|+
