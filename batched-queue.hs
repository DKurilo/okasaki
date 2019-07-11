module Main where

import Prelude hiding (head, tail)
import BatchedQueue

main :: IO ()
main = do
    let q = foldr (flip snoc) empty [1..10] :: BatchedQueue Int
    print q
    print . isEmpty $ q
    print . isEmpty $ (empty :: BatchedQueue (BatchedQueue String))
    print . head $ q
    print . tail $ q
    print . head $ snoc (foldl (\q _ -> tail q) q [1..5]) 100
    print $ snoc (foldl (\q _ -> tail q) q [1..5]) 100
