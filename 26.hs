-- This doesn't give the full answer, but you can just read through the high numbers and see its 983
main = print $ filter (\n -> (length $ show n) > 6) $ take 10 [ 1 / n | n <- [2..999] ]
