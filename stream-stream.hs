module Main where

import Stream as S

main :: IO ()
main = do
    let s = foldr (S.<:>) S.Nil [1..100] :: Stream Int
    let si = foldr (<:>) S.Nil [1..] :: Stream Int
    print . S.take 10 $ s
    print . S.drop 90 $ s
    print . S.take 5 . S.reverse $ s
    print . S.take 10 $ si
    print . S.take 10 . S.drop 10 $ si
    print . S.take 10 . S.drop 95 $ s S.++ si
