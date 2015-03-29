--Special Pythagorean triplet

find [] = []
find (x:xs) =
    findB x b
    where
        b = [x..998]
        findB a1 [] = find xs
        findB a1 (b1:b)
            | a1 + b1 + c == 1000 && (a1^2 + b1^2 == c^2) = [a1, b1, c]
            | otherwise = findB a1 b
            where
                c = 1000 - a1 - b1

main = do
    print $ foldl1 (*) $ find [1..998]
