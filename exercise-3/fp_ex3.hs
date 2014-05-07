-- authors: Frederik Zwilling (304314)
--          Roman Feldhoff (307750)
--          Alex Lorenz (309599)
--          Tom Janson (310271)

-- Exercise sheet 3
import Data.List
import Data.Ratio

-- E1
-- a)
type Pupil = String -- the pupil's name
type Group = Int

count :: Int -> [Pupil] -> [(Group, Pupil)]
count n = zip xs where xs = 1:map ((1+) . (`mod` n)) xs

count' n = zip $ cycle [1..n] -- way nicer!

-- Test: count 3 ["Pete", "Frank", "Sandra", "Caro", "Max"]
-- >> [(1,"Pete"),(2,"Frank"),(3,"Sandra"),(1,"Caro"),(2,"Max")]

-- b)
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

-- b)
-- our solution from Exercise 2/1
-- a
data IndexedTree a = Leaf [a] | Node [(IndexedTree a, a)] deriving Show
-- b
treeToList :: IndexedTree a -> [a]
treeToList (Leaf xs) = xs
treeToList (Node [(x, _)]) = treeToList x
treeToList (Node ((x, _):xs)) = (treeToList x) ++ (treeToList (Node xs))
bsp = Node [(Leaf [1,2], 1), (Node [(Leaf [6,7,8], 6)], 6) ]
-- c
contains :: Ord a => a -> IndexedTree a -> Bool
contains e (Leaf xs) = elem e xs
contains e (Node [(t,n)]) = (e == n) || contains e t
contains e (Node ((t1,n1):(t2,n2):xs))
    | e < n1    = False
    | e == n1   = True
    | e < n2    = contains e t1
    | otherwise = contains e (Node ((t2, n2):xs))

-- solution of this exercise:
contains' :: Ord a => a -> [IndexedTree a] -> Bool
contains' x ys = foldr (\y b -> contains x y || b) False ys'
  where ys' = [t | t <- ys , (treeToList t) == (sort (treeToList t))]

-- c)
leapYears = [ x | x <- [1582..], x `isDivBy` 4, not (x `isDivBy` 100)  || x `isDivBy` 400 ]
                where isDivBy = (\x y -> 0 == mod x y)
