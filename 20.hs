import Data.Char

factorial 0 = 1
factorial n = n * factorial (n - 1)

main = print $ sum $ map digitToInt $ show $ factorial 100
