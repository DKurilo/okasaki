module Main where

import System.IO
import SkewBinaryRandomAccessList as S

main :: IO()
main = do
    let l = foldl (flip cons) empty [0..16]
    print . S.lookup l $ 5
    let l1 = update l 5 100
    print . S.lookup l1 $ 5
    print l1
