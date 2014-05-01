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
