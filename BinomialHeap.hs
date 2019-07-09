module BinomialHeap
    ( Tree
    , Heap
    , empty
    , isEmpty
    , insert
    , merge
    , findMin
    , deleteMin
    ) where

import System.IO

data Tree a = Node Int a [Tree a]
    deriving (Show)
type Heap a = [Tree a]

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

rank :: Tree a -> Int
rank (Node r _ _) = r
root :: Tree a -> a
root (Node _ x _) = x

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node r' x2 c2)
    | r /= r' = error "link: different ranks"
    | x1 <= x2 = Node (r + 1) x1 (t2:c1)
    | otherwise = Node (r + 1) x2 (t1:c2)

insTree :: (Ord a) => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t1 ts@(t2:rest)
    | rank t1 < rank t2 = t1:ts
    | otherwise = insTree (link t1 t2) rest

insert :: (Ord a) => a -> Heap a -> Heap a
insert x = insTree (Node 0 x empty)

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge ts [] = ts
merge [] ts = ts
merge (t1:ts1) (t2:ts2)
    | rank t1 < rank t2 = t1:merge ts1 (t2:ts2)
    | rank t1 > rank t2 = t2:merge (t1:ts1) ts2
    | otherwise = insTree (link t1 t2) (merge ts1 ts2)

findMin :: (Ord a) => Heap a -> a
findMin [] = error "findMin: empty heap"
findMin [t] = root t
findMin (t:ts)
    | x < y = x
    | otherwise = y
    where x = root t
          y = findMin ts

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin [] = error "deleteMin: empty heap"
deleteMin ts = merge (reverse ts1) ts2
    where (Node _ _ ts1, ts2) = getMin ts
          getMin :: (Ord a) => Heap a -> (Tree a, Heap a)
          getMin [t] = (t, [])
          getMin (t:ts)
              | root t < root t' = (t, ts)
              | otherwise = (t', t:ts')
              where (t', ts') = getMin ts
