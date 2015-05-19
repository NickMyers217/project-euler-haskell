import Data.Char

sums :: [Int]
sums = [ n | n <- [2..1000000], sumDigits n 5 == n ]
    where sumDigits num exp = sum $ map (\d -> (digitToInt d) ^ exp) $ show num

main = print $ sum sums
