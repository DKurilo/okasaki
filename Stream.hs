module Stream (Streams(..), Stream(..), (<:>)) where

import Prelude hiding ((++), take, drop, reverse)

class Streams s where
    (++) :: s a -> s a -> s a
    take :: Int -> s a -> s a
    drop :: Int -> s a -> s a
    reverse :: s a -> s a

data Stream a = Nil | Cons a (Stream a)
    deriving (Show)

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons

infixr 5 <:>

instance Streams Stream where
    (++) Nil t = t
    ~(Cons x s') ++ t = Cons x $ s' ++ t

    take 0 _ = Nil
    take _ Nil = Nil
    take n ~(Cons x s') = Cons x $ take (n - 1) s'

    drop 0 s = s
    drop _ Nil = Nil
    drop n ~(Cons _ s') = drop (n - 1) s'

    reverse s = reverse' s Nil
        where reverse' :: Stream a -> Stream a -> Stream a
              reverse' Nil s' = s'
              reverse' ~(Cons x s) s' = reverse' s $ Cons x s'
