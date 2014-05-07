-- authors: Frederik Zwilling (304314)
--          Roman Feldhoff (307750)
--          Alex Lorenz (309599)
--          Tom Janson (310271)

-- Exercise sheet 3
import Data.List(inits)

-- E1
-- a)
type Pupil = String -- the pupil's name
type Group = Int

count :: Int -> [Pupil] -> [(Group, Pupil)]
count n = zip xs where xs = 1:map ((1+) . (`mod` n)) xs
-- Test: count 3 ["Pete", "Frank", "Sandra", "Caro", "Max"]
-- >> [(1,"Pete"),(2,"Frank"),(3,"Sandra"),(1,"Caro"),(2,"Max")]

-- b) TODO
-- cantor :: Int -> Rational

-- E2
-- a) TODO

-- prefixsum :: [Int] -> [Int]
-- list comprehension version

prefixsum' :: [Int] -> [Int]
prefixsum' = map sum . tail . inits
-- Test: prefixsum' [2,4,5,0,1]
-- >> [2,6,11,11,12]

-- b) TODO
-- contains' :: Ord a => a -> [IndexedTree a] -> Bool
-- contains' x ys = foldr (\y b -> contains x y || b) False ys' where ys' = -- TODO

-- c) TODO
