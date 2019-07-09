module Main where

import System.IO
import Heap hiding (Heap(..))
import BinomialHeap

main :: IO()
main = do
  let h1 = foldl (flip insert) (empty :: BinomialHeap Int) $ take 10 [1,3..]
  let h2 = foldl (flip insert) empty $ reverse . take 10 $ [4,5..]
  let h3 = merge h1 h2
  print h1
  print h2
  print . findMin $ h3
  print . findMin . deleteMin $ h3
  print . deleteMin . deleteMin $ h3
  print $ merge h3 h3
  print . decompose $ merge h3 h3
