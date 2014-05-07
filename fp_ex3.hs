-- authors: Frederik Zwilling (304314)
--          Roman Feldhoff (307750)
--          Alex Lorenz (309599)
--          Tom Janson (310271)

-- Exercise sheet 3
import Data.List(inits)
import Data.Ratio

-- E1
-- a)
type Pupil = String -- the pupil's name
type Group = Int

count :: Int -> [Pupil] -> [(Group, Pupil)]
count n = zip xs where xs = 1:map ((1+) . (`mod` n)) xs

count' n = zip (cycle [1..n]) -- way nicer!

-- Test: count 3 ["Pete", "Frank", "Sandra", "Caro", "Max"]
-- >> [(1,"Pete"),(2,"Frank"),(3,"Sandra"),(1,"Caro"),(2,"Max")]

-- b)
-- numbering starts at 0, and we'll leave it that way
cantor :: Int -> Rational
cantor n = cantor' !! (n-1) 

cantor' :: [Rational]
cantor' = concatMap cantorDiag [1..]

-- calculates the diagonal of the Cantor triangle that
-- starts with (1/n) and ends with (n/1)
cantorDiag :: Integer -> [Rational]
cantorDiag n = [a % b | a <- [1..n], b <- [n+1-a] , gcd a b == 1]

-- E2
-- a)

-- list comprehension version
prefixsum :: [Int] -> [Int]
prefixsum l = [ sum x | x <- tail $ inits l ] 
--prefixsum l = [ sum x | x <- inits l, not $ null x ] 

prefixsum' :: [Int] -> [Int]
prefixsum' = map sum . tail . inits

-- Test: prefixsum [2,4,5,0,1]
-- >> [2,6,11,11,12]

-- b) TODO
-- contains' :: Ord a => a -> [IndexedTree a] -> Bool
-- contains' x ys = foldr (\y b -> contains x y || b) False ys' where ys' = -- TODO

-- c) TODO
