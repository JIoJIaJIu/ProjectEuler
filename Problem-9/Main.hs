--Special Pythagorean triplet
number = 1000

find [] = []
find (x:xs) =
    findB x b
    where
        b = [x..(number - 2)]
        findB a1 [] = find xs
        findB a1 (b1:b)
            | a1^2 + b1^2 == c^2 = [a1, b1, c]
            | otherwise = findB a1 b
            where
                c = number - a1 - b1

main = do
    print $ product $ find [1..(number - 2)]
