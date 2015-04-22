{-# OPTIONS_GHC -O2 #-}
--Largest prime factor
--https://wiki.haskell.org/Prime_numbers

getDivisors :: Int -> [Int]
getDivisors n =
    fmap (\x -> n `div` x) $ filter (\x -> n `mod` x == 0) [2, 3 .. end]
    where
        end = n `div` 2

l :: Int -> [Int]
l n = [1, 2, 3]

getPrime :: Int -> Int
getPrime n =
    findPrime . getDivisors $ n
    where
        findPrime [] = 0
        findPrime (x:xs) =
            del x 2
            where
                del val d
                    | d >= val `div` 2 = x
                    | val `mod` d == 0 = findPrime xs
                    | otherwise = del val (d + 1)

main = do
    print $ getPrime 600851475143
