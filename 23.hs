import Data.List
import Data.IntSet (toList, fromList)

sumProperDivisors n = sum [ d | d <- [1..n-1], n `mod` d == 0 ]

main = print $ sum [1..20161] - (sum $ nub' $ genSums abundants)
  where nub'       = toList . fromList
        abundants  = filter (\x -> x < sumProperDivisors x) [12..20149]
        genSums xs = [(x + y) | x <- xs, y <- xs, y >= x, (x + y) < 20162]
