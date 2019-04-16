module HW1types where

import Data.List (nub,sort,delete)

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub

type Bag a = [(a,Int)]

-- Exercise 1 Programming With Lists
-- -- a)
-- data Bag a = EmptyBag | ListBag [(a, Integer)] deriving (Eq, Show)
--
-- emptyBag :: Bag a
-- emptyBag = EmptyBag

-- ins :: Eq a => a -> Bag a -> Bag a
-- ins element EmptyBag = ListBag [(element,1)]
-- ins element (ListBag bag)
--   | element `elem` map fst bag = ListBag bag
-- ins d [] = [d]
-- ins d ((n,i):xs) = (d, i+1):(n,i+1):xs

-- ins :: Eq a => a -> Bag a -> Bag a
-- ins x [] =
-- ins x (y:ys) =

-- myList = map (2*) [1..10]

-- -- b)
del :: Eq a => a -> Bag a -> Bag a
del _ [] = []
del x (y:ys) | x == (fst y) = del x ys
             | otherwise = y : del x ys

-- -- c)
-- bag :: Eq a => [a] -> Bag a -> Bag a
--
-- -- d)
-- subbag :: Eq a => Bag a -> Bag a -> Bool
--
-- -- e)
-- isbag :: Eq => Bag a -> Bag a -> Bag a
--
-- -- f)
-- size :: Bag a -> Int

-- Exercise 2 Graphs

-- a)
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm((fst x: nodes xs) ++ (snd x: nodes xs))

-- b)
suc :: Node -> Graph -> [Node]
suc z [] = []
suc z (x:xs) | z == fst x = snd x: suc z xs
             | z /= fst x = suc z xs -- possibly change to otherwise
-- suc z (x:xs) = if z == fst x
--                then snd x: suc z xs
--                else suc z xs

-- c)
detach :: Node -> Graph -> Graph
detach z [] = []
detach z (x:xs) | z /= fst x && z /= snd x = x: detach z xs
                | otherwise = detach z xs
                -- | z == fst x || z == snd x = detach z xs
-- detach z (x:xs) = if z /= fst x && z /= snd x
--                   then x: detach z xs
--                   else detach z xs

-- d)

-- cycHelper :: [Int] -> Graph
-- cycHelper x =

cyc :: Int -> Graph
cyc 0 = []
cyc x = zip [1..x] ([2..x] ++ [1])

-- cyc x | x == x = zip [x] [1] ++ zip [x-1] [x] ++ cyc (x-1)
--       | x == 1 = cyc (x-1)
--       | otherwise = zip [x-1] [x] ++ cyc (x-1)

-- cycleList = take 50 (cycle [1,2,3,4,5])

        -- if x == 4
        -- then zip [x] [1] ++ zip [x-1] [x] ++ cyc (x-1)
        -- else if x == 1
        -- then cyc (x-1)
        -- else zip [x-1] [x] ++ cyc (x-1)

-- Exercise 3 Programming with Data Types

type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point,Point)

-- a)
width :: Shape -> Length
width (Pt _) = 0
width (Circle _ i) = 2*i
width (Rect _ i _) = i

-- b)
bbox :: Shape -> BBox
bbox (Pt i) = (i,i)
bbox (Circle i j) = (((fst i) - j, (snd i) - j),((fst i) + j, (snd i) + j))
bbox (Rect i j k) = (i,(((fst i)+j),((snd i)+k)))

-- c)
minX :: Shape -> Number
minX (Pt i) = fst i
minX (Circle i j) = (fst i) - j
minX (Rect i j k) = fst i

-- d)

-- move helper
addPt :: Point -> Point -> Point
addPt i j = ((fst i)+(fst j),(snd i)+(snd j))

move :: Shape -> Point -> Shape
move (Pt i) j = Pt (addPt i j)
move (Circle i j) l = Circle (addPt i l) j
move (Rect i j k) m = Rect (addPt i m) j k

-- e)

-- moveToX :: Number -> Shape -> Shape
--
-- alignLeft :: Figure -> Figure
-- alignLeft x :: Pt (1,1)

-- f)
-- inside :: Shape -> Shape -> Bool
-- inside (Pt i_1) (Circle i_2 j_2)
-- inside (Pt i_1) (Rect i_2 j_2 k_2)
-- inside (Circle i_1 j_1) (Pt i_2)
-- inside (Circle i_1 j_1) (Rect i_2 j_2 k_2)
-- inside (Rect i_1 j_1 k_1) (Pt i_2)
-- inside (Rect i_1 j_1 k_1) (Circle i_2 j_2)
