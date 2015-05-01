--Problem 1
euler1 cap = sum [ n | n <- [1..cap - 1], mod n 3 == 0 || mod n 5 == 0 ]

--Problem 2
fibs = 1 : 2 : [ a + b | (a, b) <- zip fibs (tail fibs) ]
euler2 cap = sum [ n | n <- takeWhile (< cap) fibs, n `mod` 2 == 0 ]

