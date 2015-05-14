numSpiral :: [Int] -> Int -> Int -> Int -> [Int]
numSpiral acc mult diag cap
    | ((length $ acc) - 1) `div` 4 >= cap = acc
    | diag > 4 = numSpiral acc (mult + 2) 1 cap
    | otherwise = numSpiral (nextAcc acc) mult (diag + 1) cap
    where nextAcc curAcc = curAcc ++ (last curAcc + mult) : []

main :: IO ()
main = print $ sum $ numSpiral [1] 2 1 500
