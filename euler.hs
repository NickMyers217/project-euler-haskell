--Problem 1
    --List comprehension
    euler1 :: (Integral a) => a -> a
    euler1 cap = sum [ n | n <- [1..cap - 1], mod n 3 == 0 || mod n 5 == 0 ]

    --Recursion with guards
    euler1' :: (Integral a) => a -> a -> a
    euler1' n cap
        | n == cap                         = 0
        | n `mod` 3 == 0 || n `mod` 5 == 0 = n + euler1' (n + 1) cap
        | otherwise                        = euler1' (n + 1) cap


--Problem 2
    fibSum :: (Integral a) => a -> a -> a -> a
    fibSum a b cap
        | c >= cap       = 0
        | c `mod` 2 == 0 = c + fibSum b c cap
        | otherwise      = fibSum b c cap
        where c = a + b

    euler2 :: (Integral a) => a -> a
    euler2 n = fibSum 1 1 n


--Problem 3
    lgstPrimeFac :: (Integral a) => a -> a -> a
    lgstPrimeFac num factor
        | factor^2 > num        = num
        | num `mod` factor == 0 = lgstPrimeFac (num `div` factor) factor
        | otherwise             = lgstPrimeFac num (factor + 1)

    euler3 :: (Integral a) => a -> a
    euler3 n = lgstPrimeFac n 2


--Problem 4
    euler4 = lgstPalindrome 100 999
    lgstPalindrome strt cap = maximum (filter isPalindrome [ a * b | a <- [strt..cap], b <- [a..cap] ])
        where isPalindrome n = show n == reverse (show n)

--Problem 5
    --Came up with this solution first
    isDivByAll :: (Integral a) => a -> [a] -> Bool
    isDivByAll n [] = True
    isDivByAll n (x:xs)
        | n `mod` x /= 0 = False
        | otherwise      = n `isDivByAll` xs

    euler5 :: (Integral a) => a -> [a] -> a
    euler5 n divisors
        | n `isDivByAll` divisors = n
        | otherwise               = euler5 (n + 1) divisors

    --It can be done with this one liner though
    --It folds the least common multiple function across the list of divisors
    --Normally it takes in two inputs and finds the lcm, now it finds the lcm of the whole list
    euler5' :: (Integral a) => [a] -> a
    euler5' divisors = foldr lcm 1 divisors

--Problem 6
    euler6 :: (Integral a) => [a] -> a
    euler6 nums = (sum nums ^2) - (sum [ n ^2 | n <- nums ])

--Problem 7
    --This one is slow becuase it has to do the whole list comprehension
    --If it used recursion it could just break out and return false on the first divisor
    isPrime :: (Integral a) => a -> Bool
    isPrime 1 = False
    isPrime 2 = True
    isPrime 3 = True
    isPrime n
        | even n                          = False
        | length oddDivisorsBelowRoot > 0 = False
        | otherwise                       = True
        where oddDivisorsBelowRoot = [ x | x <- [3,5..truncate (sqrt (fromIntegral n)) + 1], n `mod` x == 0]

    euler7 :: (Integral a) => Int -> a
    euler7 target = [ n | n <- [2..], isPrime n]!!(target - 1)
