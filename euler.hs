--Problem 1
	--List comprehension
	euler1 :: (Integral a) => a -> a
	euler1 cap = sum [ n | n <- [1..cap - 1], mod n 3 == 0 || mod n 5 == 0 ]

	--Recursion with guards
	euler1' :: (Integral a) => a -> a -> a
	euler1' n cap
		| n == cap 							= 0
		| n `mod` 3 == 0 || n `mod` 5 == 0 	= n + euler1' (n + 1) cap
		| otherwise 						= euler1' (n + 1) cap


--Problem 2
	fibSum :: (Integral a) => a -> a -> a -> a
	fibSum a b cap
		| c >= cap 			= 0
		| c `mod` 2 == 0 	= c + fibSum b c cap
		| otherwise			= fibSum b c cap
		where c = a + b

	euler2 :: (Integral a) => a -> a
	euler2 n = fibSum 1 1 n


--Problem 3
	lgstPrimeFac :: (Integral a) => a -> a -> a
	lgstPrimeFac num factor
		| factor^2 > num		= num
		| num `mod` factor == 0	= lgstPrimeFac (num `div` factor) factor
		| otherwise				= lgstPrimeFac num (factor + 1)

	euler3 :: (Integral a) => a -> a
	euler3 n = lgstPrimeFac n 2


--Problem 4
	euler4 = lgstPalindrome 100 999
	lgstPalindrome strt cap = maximum (filter isPalindrome [ a * b | a <- [strt..cap], b <- [a..cap] ])
		where isPalindrome n = show n == reverse (show n)

