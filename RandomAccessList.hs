module RandomAccessList (RandomAccessList(..)) where

import Prelude hiding (tail, head, lookup)

class RandomAccessList l where
    empty :: l a
    isEmpty :: l a -> Bool

    cons :: a -> l a -> l a
    head :: l a -> a
    tail :: l a -> l a

    lookup :: l a -> Int -> a
    update :: l a -> Int -> a -> l a
