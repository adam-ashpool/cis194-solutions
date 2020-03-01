module Homework where

import           Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . subtract 2) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum .
  filter (even) .
  takeWhile (/= 1) .
  iterate
    (\x ->
       if even x
         then x `div` 2
         else 3 * x + 1)

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

maxTreeHeight :: Integer -> Integer
maxTreeHeight = floor . logBase 2 . fromIntegral

{-
-- should be recursive, not correct
hasSpace :: Tree a -> Bool
hasSpace (Node h Leaf _ _) = h > 1
hasSpace (Node h _ _ Leaf) = h > 1
hasSpace _                 = False

-- not working correctly, filling the tree downward is not recursive
insertItem :: a -> Tree a -> Tree a
insertItem x Leaf = Node 0 Leaf x Leaf
insertItem x tree@(Node h ltree@(Leaf) y rtree@(Leaf)) = Node (h + 1) tree x rtree
insertItem x (Node h ltree@(Leaf) y rtree@(Node _ _ _ _)) =
  Node h (insertItem x ltree) y rtree
insertItem x (Node h ltree@(Node _ _ _ _) y rtree@(Leaf)) =
  Node h ltree y (insertItem x rtree)
insertItem x tree@(Node h ltree@(Node lh _ _ _) y rtree@(Node rh _ _ _))
  | hasSpace ltree = Node h (insertItem x ltree) y rtree
  | hasSpace rtree = Node h ltree y (insertItem x rtree)
  | otherwise = Node (h + 1) Leaf x tree

foldTree :: [a] -> Tree a
foldTree = foldr insertItem Leaf
-}
hasSpace :: Tree a -> Bool
hasSpace (Node h Leaf _ _)      = h > 0
hasSpace (Node h _ _ Leaf)      = h > 0
hasSpace (Node _ ltree _ rtree) = hasSpace ltree || hasSpace rtree
hasSpace _                      = False

insertItem :: Integer -> a -> Tree a -> Tree a
insertItem h x Leaf = Node h Leaf x Leaf
insertItem _ x (Node h Leaf y Leaf) = Node h (insertItem (h - 1) x Leaf) y Leaf
insertItem _ x (Node h ltree@(Node _ _ _ _) y Leaf) =
  Node h ltree y (insertItem (h - 1) x Leaf)
insertItem _ x (Node h Leaf y rtree@(Node _ _ _ _)) =
  Node h (insertItem (h - 1) x Leaf) y rtree
insertItem _ x (Node h ltree@(Node _ _ _ _) y rtree@(Node _ _ _ _))
  | hasSpace ltree = Node h (insertItem h x ltree) y rtree
  | hasSpace rtree = Node h ltree y (insertItem h x rtree)

foldTree :: [a] -> Tree a
foldTree xs =
  foldr (insertItem (maxTreeHeight (fromIntegral (length xs)))) Leaf xs

xor :: [Bool] -> Bool
xor =
  foldr
    (\x acc ->
       if x
         then not acc
         else acc)
    False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

eliminatedNumbers :: Integer -> [Integer]
eliminatedNumbers n =
  (filter (<= n) .
   map (\(i, j) -> i + j + 2 * i * j) .
   filter (\(i, j) -> j >= i) . cartProd [1 .. n])
    [1 .. n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1 .. n] \\ (eliminatedNumbers n)
