main = print $ snd $ head $ dropWhile (\(n, i) -> (length $ show n) < 1000) fibsWithIndices
    where fibsWithIndices = fibs `zip` [2..]
              where fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

