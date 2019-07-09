module SkewBinomialHeap (SkewBinomialHeap, Heap(..)) where

import Heap

data Tree a = Node Int a [a] [Tree a]
    deriving (Show)
newtype SkewBinomialHeap a = SBH [Tree a]
    deriving (Show)

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

insTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t1 ts@(t2:rest)
    | r1 < r2 = t1:ts
    | r1 > r2 = t2:insTree t1 rest
    | otherwise = insTree (link t1 t2) rest
    where r1 = rank t1
          r2 = rank t2

mergeTrees :: (Ord a) => [Tree a] -> [Tree a] -> [Tree a]
mergeTrees t1 [] = t1
mergeTrees [] t2 = t2
mergeTrees (t1:ts1) (t2:ts2)
    | r1 < r2 = t1:mergeTrees ts1 (t2:ts2)
    | r1 > r2 = t2:mergeTrees (t1:ts1) ts2
    | otherwise = insTree (link t1 t2) $
                  mergeTrees ts1 ts2
    where r1 = rank t1
          r2 = rank t2

normalize :: (Ord a) => [Tree a] -> [Tree a]
normalize [] = []
normalize (t:ts) = insTree t ts

ins :: (Ord a) => a -> [Tree a] -> [Tree a]
ins x ts@(t1:t2:rest)
    | r1 == r2 = skewLink x t1 t2:rest
    where r1 = rank t1
          r2 = rank t2
ins x ts = Node 0 x [] []:ts

removeMinTree :: (Ord a) => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "removeMinTree: empty Heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts)
    | root t <= root t' = (t, ts)
    | otherwise = (t', t:ts')
    where (t', ts') = removeMinTree ts

insertAll :: (Ord a) => [a] -> [Tree a] -> [Tree a]
insertAll = flip $ foldl (flip ins)
--           insertAll [] ts = ts
--           insertAll (x:xs) ts = insertAll xs $ insert x ts

instance Heap SkewBinomialHeap where
    empty = SBH []
    isEmpty (SBH ts) = null ts

    insert x (SBH ts) = SBH $ ins x ts
    merge (SBH ts1) (SBH ts2) = SBH $ mergeTrees (normalize ts1) (normalize ts2)

    findMin (SBH ts) = root t
        where (t, _) = removeMinTree ts
    deleteMin (SBH ts) = SBH $ insertAll xs $ mergeTrees (reverse c) (normalize ts')
        where (Node _ x xs c, ts') = removeMinTree ts
