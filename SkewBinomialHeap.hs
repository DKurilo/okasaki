module SkewBinomialHeap
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

data Tree a = Node Int a [a] [Tree a]
    deriving (Show)
type Heap a = [Tree a]

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

rank :: Tree a -> Int
rank (Node r _ _ _) = r
root :: Tree a -> a
root (Node _ x _ _) = x

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 xs1 c1) t2@(Node r' x2 xs2 c2)
    | r /= r' = error "link: different ranks"
    | x1 <= x2 = Node (r + 1) x1 xs1 (t2:c1)
    | otherwise = Node (r + 1) x2 xs2 (t1:c2)

skewLink :: (Ord a) => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2
    | x <= y = Node (r + 1) x (y:ys) c
    | otherwise = Node (r + 1) y (x:ys) c
    where (Node r y ys c) = link t1 t2

insTree :: (Ord a) => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t1 ts@(t2:rest)
    | r1 < r2 = t1:ts
    | r1 > r2 = t2:insTree t1 rest
    | otherwise = insTree (link t1 t2) rest
    where r1 = rank t1
          r2 = rank t2

mergeTrees :: (Ord a) => Heap a -> Heap a -> Heap a
mergeTrees t1 [] = t1
mergeTrees [] t2 = t2
mergeTrees (t1:ts1) (t2:ts2)
    | r1 < r2 = t1:mergeTrees ts1 (t2:ts2)
    | r1 > r2 = t2:mergeTrees (t1:ts1) ts2
    | otherwise = insTree (link t1 t2) $
                  mergeTrees ts1 ts2
    where r1 = rank t1
          r2 = rank t2

normalize :: (Ord a) => Heap a -> Heap a
normalize [] = []
normalize (t:ts) = insTree t ts

insert :: (Ord a) => a -> Heap a -> Heap a
insert x ts@(t1:t2:rest)
    | r1 == r2 = skewLink x t1 t2:rest
    where r1 = rank t1
          r2 = rank t2
insert x ts = Node 0 x [] []:ts

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge ts1 ts2 = mergeTrees (normalize ts1)
                           (normalize ts2)

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
deleteMin ts = insertAll xs $ mergeTrees (reverse c)
                                         (normalize ts')
    where getMin :: (Ord a) => Heap a -> (Tree a, Heap a)
          getMin [t] = (t, [])
          getMin (t:ts)
              | root t <= root t' = (t, ts)
              | otherwise = (t', t:ts')
              where (t', ts') = getMin ts
          (Node _ x xs c, ts') = getMin ts
          insertAll :: (Ord a) => [a] -> Heap a -> Heap a
          insertAll = flip $ foldl (flip insert)
--           insertAll [] ts = ts
--           insertAll (x:xs) ts = insertAll xs $
--                                           insert x ts
