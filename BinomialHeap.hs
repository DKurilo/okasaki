module BinomialHeap (BinomialHeap, Heap(..)) where

import Heap 

data Tree a = Node Int a [Tree a]
    deriving (Show)
newtype BinomialHeap a = BH [Tree a]
    deriving (Show)

rank :: Tree a -> Int
rank (Node r _ _) = r
root :: Tree a -> a
root (Node _ x _) = x

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node r' x2 c2)
    | r /= r' = error "link: different ranks"
    | x1 <= x2 = Node (r + 1) x1 (t2:c1)
    | otherwise = Node (r + 1) x2 (t1:c2)

insTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t1 ts@(t2:rest)
    | rank t1 < rank t2 = t1:ts
    | otherwise = insTree (link t1 t2) rest

mrg :: (Ord a) => [Tree a] -> [Tree a] -> [Tree a]
mrg ts [] = ts
mrg [] ts = ts
mrg (t1:ts1) (t2:ts2)
    | rank t1 < rank t2 = t1:mrg ts1 (t2:ts2)
    | rank t1 > rank t2 = t2:mrg (t1:ts1) ts2
    | otherwise = insTree (link t1 t2) (mrg ts1 ts2)

removeMinTree :: (Ord a) => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "removeMinTree: heap can't be empty"
removeMinTree [t] = (t,[])
removeMinTree (t:ts)
    | root t <= root t' = (t, ts)
    | otherwise = (t', t:ts')
    where (t',ts') = removeMinTree ts

instance Heap BinomialHeap where
    empty = BH []
    isEmpty (BH ts) = null ts

    insert x (BH ts) = BH $ insTree (Node 0 x []) ts
    merge (BH ts1) (BH ts2) = BH $ mrg ts1 ts2

    findMin (BH ts) = root t
        where (t, _) = removeMinTree ts
    deleteMin (BH ts) = BH $ mrg (reverse ts1) ts2
        where (Node _ _ ts1, ts2) = removeMinTree ts
