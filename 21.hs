getDivisors :: Int -> [Int]
getDivisors n = [ d | d <- [1..n - 1], n `mod` d == 0 ]

isAmicable :: Int -> Bool
isAmicable a = (sum $ getDivisors b) == a && a /= b
    where b = sum $ getDivisors a

main = print $ sum $ filter isAmicable $ [1..10000]
