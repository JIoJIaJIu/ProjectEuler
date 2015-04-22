import Data.List.Ordered(minus)

primes :: Int -> [Int]
primes n =
    sieve [2,3..n]
    where
        sieve (x:xs)
            | x * x > n = x : xs
            | otherwise = x : sieve (xs `minus` [x*x,x*x + x..])

main = do
    print $ sum $ primes 2000000
