{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module HigherOrder where

import           Data.List (foldl', sortBy)
import           Prelude   hiding (all, product, reverse, take)


-- These are binary trees with labels in their nodes.

data BinTree a =
    Bin (BinTree a) a (BinTree a)
  | Empty
  deriving (Eq, Show)

instance Functor BinTree where
  fmap = mapBinTree

instance Foldable BinTree where
  foldr = foldrBinTree

-- Task HigherOrder-1.
--
-- Define 'product' both using an accumulator explicitly,
-- and using (strict) foldl'.

-- |
-- >>> product [1 .. 4]
-- 24
--
product :: Num a => [a] -> a
product = go 1
  where
    go acc []     = acc
    go acc (x:xs) = go (acc * x) xs


-- |
-- >>> product [3, 11]
-- 33
--
product' :: Num a => [a] -> a
product' = foldl' (*) 1


-- Task HigherOrder-2.
--
-- Define 'reverse' using 'foldl'.

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []


-- Task HigherOrder-3.
--
-- Define a Functor instance for binary trees. For
-- this, we have to define a map function on binary
-- trees and then define the class instance.
--
-- The instance is actually given below. You just
-- have to uncomment it.

-- |
-- >>> mapBinTree (+1) (Bin Empty 7 (Bin Empty 8 Empty))
-- Bin Empty 8 (Bin Empty 9 Empty)
--
mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _  Empty      = Empty
mapBinTree f (Bin l x r) = Bin (mapBinTree f l) (f x) (mapBinTree f r)


-- Task HigherOrder-4.
--
-- The 'BinTree' type is suitable for representing
-- "binary search trees".
--
-- Binary search trees are trees that store their elements
-- in order, so that we can efficiently find elements by comparing
-- the element we are looking for with the current node, and
-- descending either left or right.
--
-- Define a function 'isBST' that checks if a given 'BinTree'
-- is a binary search tree.
-- Bin Empty 0 (Bin (Bin Empty 0 Empty) 1 Empty)
--
-- Note: Accepts duplicate values


isBST :: Ord a => BinTree a -> Bool
isBST Empty = True
isBST (Bin l x r) = (go x (<) l) && (go x (>) r)
  where
    go _ _ Empty             = True
    go prev f (Bin l' x' r') = f x' prev && (go x' (<) l') && (go x' (>) r')


-- Task HigherOrder-5.
--
-- Define a function 'search' that looks up a value in a BST.
--
-- From now on, we use a type synonym to signal that a certain
-- binary tree should in fact be a binary search tree, even if
-- the type system does not actively enforce this.

type BST a = BinTree a

search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search x (Bin l x' r)
  | x > x' = search x r
  | x < x' = search x l
  | otherwise = True


-- Task HigherOrder-6.
--
-- Define a function 'insert' that inserts a value into a BST
-- while maintaining the BST property. (Don't worry about balancing
-- the tree. That's not important for now. But do make sure you
-- maintain the BST property itself.)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Bin Empty x Empty
insert x (Bin l x' r)
  | x > x' = Bin l x' (insert x r)
  | x < x' = Bin (insert x l) x' r
  | otherwise = Bin l x r


-- Task HigherOrder-7.
--
-- Define the function 'all' (as in the Prelude) using 'foldr'.
-- Hide the original binding from the Prelude by exluding it in
-- the module header. Provide the type signature yourself.

-- |
-- >>> all even [2, 4 .. 20]
-- True
--
-- >>> all odd [1, 1, 1, 2, 3, 3]
-- False
--
all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\ x acc -> acc && f x) True


-- Task HigherOrder-8.
--
-- Import the function 'sortBy' from the 'Data.List' module.
-- Then use this function to define a function that sorts a
-- list in descending rather than ascending order.

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy desc
  where
    desc x y
      | x < y = GT
      | x > y = LT
      | otherwise = EQ


-- Task HigherOrder-9.
--
-- Use 'insert' and 'foldr' to create a BST from a list.

fromListBST :: Ord a => [a] -> BST a
fromListBST = foldr insert Empty

-- bt1 = Bin Empty 'x' (Bin Empty 'y' Empty)
-- bt2 = Bin Empty 'x' Empty
-- bt3 = Bin (Bin Empty 1 Empty) 2 (Bin Empty 5 Empty)
-- bt4 = Bin (Bin Empty 'a' (Bin Empty 'a' Empty)) 'a' Empty


-- Task HigherOrder-10.
--
-- We want to attach unique numbers to each node in a binary
-- tree, so that all the numbers from left to right are labelled
-- in ascending order.
--
-- NOTE: This is not easy. Think about this and discuss your
-- strategy with us before you proceed.

-- |
-- >>> labelTree $ Bin Empty 'x' (Bin Empty 'y' Empty)
-- Bin Empty ('x',1) (Bin Empty ('y',2) Empty)
--
-- >>> labelTree $ Bin (Bin Empty 1 Empty) 2 (Bin Empty 5 Empty)
-- Bin (Bin Empty (1,1) Empty) (2,2) (Bin Empty (5,3) Empty)

labelTree :: BinTree a -> BinTree (a, Int)
labelTree t = snd $ go 0 t
  where
    go :: Int -> BinTree a -> (Int, BinTree (a, Int))
    go n Empty = (n, Empty)
    go n (Bin l x r) =
      let
        (n', l') = go n l
        n'' = n' + 1
        (n''', r') = go n'' r
      in
        (n''', Bin l' (x, n'') r')


-- Task HigherOrder-11.
--
-- Another form of tree labeling does not use an integer, but
-- a label supply that is given as a list. So write a variant
-- of 'labelTree' that takes the labels from a list, but uses
-- every label only once. You may assume in this function that
-- the list contains infinitely many (or at least sufficiently
-- many) labels, so you don't have to return a 'Maybe' if the
-- list is too short, but can just crash.

-- |
-- >>> labelTree' (Bin Empty 1 (Bin Empty 42 Empty)) "Haskell"
-- Bin Empty (1,'H') (Bin Empty (42,'a') Empty)
--
labelTree' :: BinTree a -> [b] -> BinTree (a, b)
labelTree' t xs = snd $ go xs t
  where
    go :: [b] -> BinTree a -> ([b], BinTree (a, b))
    go labels Empty = (labels, Empty)
    go labels (Bin l x r) =
      let
        (label:labels', l') = go labels l
        (labels'', r') = go labels' r
      in
        (labels'', Bin l' (x, label) r')


-- Task HigherOrder-12.
--
-- Define the catamorphism on 'BinTree'.
-- Also come up with the type signature yourself.
-- Look at functions such as 'mapBinTree' and 'search'
-- above for inspiration. Also try to
-- rewrite these in terms of the catamorphism once you
-- are done.
binTreeCatamorphism :: (a -> b -> c) -> (b -> b) -> BinTree a -> b -> BinTree c
binTreeCatamorphism f1 f2 t ctx = snd $ go ctx t
  where
    -- go :: b -> BinTree a -> (b, BinTree c)
    go b Empty = (b, Empty)
    go b (Bin l x r) =
      let
        (b', l') = go b l
        y = f1 x b'
        b'' = f2 b'
        (b''', r') = go b'' r
      in
        (b''', Bin l' y r')

labelTree'' :: BinTree a -> BinTree (a, Int)
labelTree'' t = binTreeCatamorphism (\ a b -> (a, b + 1)) (+1) t 0


-- Task HigherOrder-13.
--
-- Try to implement the function 'take' on lists using 'foldr'.
--
-- Once again, this is not easy, and you should discuss your
-- ideas with us before trying.
--
-- Consider the type signature and a possible definition of
-- take, and note that not just the list is being traversed,
-- but also the number changes.

-- |
-- >>> take 3 "Haskell"
-- "Has"
--
-- take :: Int -> [a] -> [a]
-- take _ []       = []
-- take n (x : xs)
--   | n > 0     = x : take (n - 1) xs
--   | otherwise = []

take :: Int -> [a] -> [a]
take n xs = map snd $ foldr keepN [] $ zip [1..] xs
  where
    keepN :: (Int, a) -> [(Int, a)] -> [(Int, a)]
    keepN (n', x) acc
      | n' <= n = (n', x) : acc
      | otherwise = acc


-- Task HigherOrder-14.
--
-- If you succeeded in defining 'take' in terms of 'foldr',
-- then perhaps it will not surprise you all that much that
-- even 'foldl' can be written in terms of 'foldr'.
--
-- Try to do this. The approach required is similar.

-- |
-- >>> myFoldl (\xs x -> xs ++ [x]) [] "Haskell"
-- "Haskell"
--
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc xs = foldr (flip f) acc (reverse xs)


-- Task HigherOrder-15.
--
-- Define a Foldable instance for binary trees.
-- For this, there are several methods, but the
-- easiest with our knowledge so far is to implement
-- a foldr function on trees.
-- For this, one option might be to first convert a
-- binary tree to a list.

foldrBinTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrBinTree f acc t = foldr f acc $ toList t
  where
    toList :: BinTree a -> [a]
    toList Empty       = []
    toList (Bin l x r) = toList l ++ [x] ++ toList r

