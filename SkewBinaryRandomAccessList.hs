module SkewBinaryRandomAccessList (SkewBinaryRandomAccessList, RandomAccessList(..)) where

import Prelude hiding (tail, head, lookup)
import RandomAccessList

data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving (Show)
newtype SkewBinaryRandomAccessList a = L [(Int, Tree a)]
    deriving (Show)

cons' :: a -> [(Int, Tree a)] -> [(Int, Tree a)]
cons' x ((w1, t1):(w2, t2):ts)
    | w1 == w2 = (1 + w1 + w2, Node x t1 t2):ts
cons' x ts = (1, Leaf x):ts

head' :: [(Int, Tree a)] -> a
head' [] = error "head: list is empty"
head' ((1, Leaf x):_) = x
head' ((_, Node x _ _):_) = x


tail' :: [(Int, Tree a)] -> [(Int, Tree a)]
tail' [] = error "tail: list is empty"
tail' ((1, Leaf _):ts) = ts
tail' ((w, Node _ t1 t2):ts) = (hw, t1):(hw, t2):ts
    where hw = w `div` 2

lookupTree :: Int -> Tree a -> Int -> a
lookupTree 1 (Leaf x) 0 = x
lookupTree 1 (Leaf _) i =
    error "lookupTree: index too big"
lookupTree w (Node x t1 t2) 0 = x
lookupTree w (Node x t1 t2) i
    | i == hw = x
    | i < hw = lookupTree hw t1 (i - 1)
    | otherwise = lookupTree hw t2 (i - 1 - hw)
    where hw = w `div` 2

updateTree :: Int -> Tree a -> Int -> a -> Tree a
updateTree 1 (Leaf x) 0 y = Leaf y
updateTree 1 (Leaf x) i y =
    error "updateTree: index too big"
updateTree w (Node x t1 t2) 0 y = Node y t1 t2
updateTree w (Node x t1 t2) i y
    | i == hw = Node y t1 t2
    | i < hw = Node x (updateTree hw t1 (i - 1) y) t2
    | otherwise = Node x t1 $
                  updateTree hw t2 (i - 1 - hw) y
    where hw = w `div` 2

lookup' :: [(Int, Tree a)] -> Int -> a
lookup' [] _ = error "lookup: index too big"
lookup' ((w, t):ts) i
    | i < w = lookupTree w t i
    | otherwise = lookup' ts (i - w)

update' :: [(Int, Tree a)] -> Int -> a -> [(Int, Tree a)]
update' [] _ _ = error "update: index too big"
update' ((w, t):ts) i y
    | i < w = (w, updateTree w t i y):ts
    | otherwise = (w, t):update' ts (i - w) y

instance RandomAccessList SkewBinaryRandomAccessList where
    empty = L []
    isEmpty (L ts) = null ts

    cons x (L ts) = L $ cons' x ts
    head (L ts) = head' ts
    tail (L ts) = L . tail' $ ts

    lookup (L ts) = lookup' ts
    update (L ts) i y = L $ update' ts i y
