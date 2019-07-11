module BatchedQueue (BatchedQueue(..), Queue(..)) where

import Prelude hiding (head, tail)
import Queue

data BatchedQueue a = BQ [a] [a]
    deriving Show

queue :: [a] -> [a] -> BatchedQueue a
queue [] r = BQ (reverse r) []
queue f r = BQ f r

instance Queue BatchedQueue where
    empty = BQ [] []
    isEmpty (BQ f r) = null f

    snoc (BQ f r) x = queue f (x:r)
    head (BQ [] _) = error "head: empty BatchedQueue"
    head (BQ (x:_) _) = x
    tail (BQ [] _) = error "tail: empty BatchedQueue"
    tail (BQ (_:f) r) = queue f r
