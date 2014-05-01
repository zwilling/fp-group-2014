-- authors: Frederik Zwilling (304314)
--          Roman Feldhoff (307750)
--          Alex Lorenz (309599)
--          Tom Janson (310271)


-- Exercise 1
-- a
data IndexedTree a = Leaf [a] | Node [(IndexedTree a, a)] deriving Show

-- b
treeToList :: IndexedTree a -> [a]
treeToList (Leaf xs) = xs
treeToList (Node [(x, y)]) = treeToList x
treeToList (Node ((x, y):xs)) = (treeToList x) ++ (treeToList (Node xs))

bsp = Node [(Leaf [1,2], 1), (Node [(Leaf [6,7,8], 6)], 6) ]

-- c
-- contains :: Ord a => a -> IndexedTree a -> Bool
-- contains e (Leaf xs) = elem e xs
-- contains e (Node [(x, y)]) | e < y = False
--                            | otherwise = contains e x
-- contains e (Node ((x, y):xs)) | e < y = False
--                               | otherwise = (contains e (Node xs)) || (contains e x)
-- nicht so schön

-- Exercise 2
-- a)
data Regex = Or Regex Regex | Kleene Regex | Concat Regex Regex | Symbol Char | Empty

exampleRegex = Or (Kleene Empty) (Concat (Symbol 'a') (Symbol 'b'))

-- b)
instance Show Regex where
  show Empty        = "{}"
  show (Symbol a)   = [a]
  show (Kleene r)   = "(" ++ show r ++ ")*"
  show (Or r s)     = "(" ++ show r ++ " | " ++ show s ++ ")"
  show (Concat r s) = show r ++ show s

-- Exercise 3

data Genre = Nonfiction | Novel | Biography deriving (Eq, Show)
type Name = (String, String)
type Date = (Int, Int, Int) -- day, month, year
data Book = ABook Genre
                  Name -- name of the author
                  String -- title of the book
                  Date -- date of publication
                  Int -- number of pages
            deriving Show
                     
genre :: Book -> Genre
genre (ABook g _ _ _ _) = g

author :: Book -> Name
author (ABook _ a _ _ _) = a

title :: Book -> String
title (ABook _ _ t _ _) = t

date :: Book -> Date
date (ABook _ _ _ d _) = d

pages :: Book -> Int
pages (ABook _ _ _ _ p) = p

year :: Book -> Int
year b = let (_, _, y) = date b in y

breakingNews = ABook Novel ("Schaetzing", "Frank") "Breaking News" (06, 03, 2014) 976
snowden = ABook Nonfiction ("Harding", "Luke") "The Snowden Files" (06, 02, 2014) 346
futureShock = ABook Nonfiction ("Toffler", "Alvin") "Future Shock" (01, 06, 1984) 576

-- a
publishedIn :: Int -> [Book] -> [Book]
publishedIn y = filter (\x -> (year x) == y)

-- b
totalPages :: [Book] -> Int
totalPages = foldr (\x -> (+) (pages x)) 0

-- c
toAuthor :: [Book] -> [Name]
toAuthor = map (\x -> author x)

-- d
titlesOf :: Genre -> [Book] -> [String]
titlesOf g bs = map (\x -> title x) (filter (\y -> (genre y) == g) bs)

-- e
pagesOf :: Genre -> [Book] -> Int
pagesOf g l = totalPages (filter (\x -> g == (genre x)) l)

-- Exercise 4
data MultTree a = AMultTree a [MultTree a] deriving Show
exMultTree = AMultTree 8 [AMultTree 3 [AMultTree (-56) [], AMultTree 4 [], AMultTree 987 []], AMultTree 4 [AMultTree 6 []]]

-- a)
mapMult :: (a -> b) -> MultTree a -> MultTree b
mapMult f (AMultTree n l)  = AMultTree (f n) (map (mapMult f) l)

-- b)
mean x y = (x + y) `div` 2

depthFirstFold :: (b -> a -> b) -> b -> MultTree a -> b
--depthFirstFold :: (a -> a -> a) -> a -> MultTree a -> a -- workaround for testing

depthFirstFold f e (AMultTree n []) = f e n
-- TODO

-- various previous attempts, all (completely??) wrong
--depthFirstFold f e (AMultTree n (l:ls)) = depthFirstFold f (depthFirstFold f e l) (AMultTree n ls)
--depthFirstFold f e (AMultTree n l) = foldr f (f e n) (map (depthFirstFold f e) l)
--depthFirstFold f e (AMultTree n l) = f (foldr f e (map (depthFirstFold f e) l)) n
