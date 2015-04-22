{-# OPTIONS_GHC -O2 #-}
import Data.List.Ordered(minus)

optimusPrime :: Int -> Int
optimusPrime n =
    sieve n [2,3..]
        where
            sieve 1 (p:xs) = p
            sieve n (p:xs) =
                sieve (n - 1) (xs `minus` [p, p+p..])
main = do
    print $ optimusPrime 6
    print $ optimusPrime 10001
