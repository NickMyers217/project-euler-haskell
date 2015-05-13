import Data.Char
import Data.List
import Data.List.Split

main = do
  names <- readFile "p022_names.txt"
  print $ sum $ map tupMul $
       (map charScore $ splitNames names) `zip` ([1..length $ splitNames names])
    where
      splitNames     = sort . splitOn "," . filter (\c -> c /= '\"')
      charScore name = sum $ map (\c -> (ord c) - 64) name
      tupMul (a, b)  = a * b
  
