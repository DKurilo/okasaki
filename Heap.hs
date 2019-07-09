module Heap (Heap(..), decompose) where

class Heap h where
    empty :: Ord a => h a
    isEmpty :: Ord a => h a -> Bool

    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a

    findMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a

decompose :: (Ord a, Heap h) => h a -> [a]
decompose h
    | isEmpty h = []
    | otherwise = x:decompose h'
    where x = findMin h
          h' = deleteMin h
