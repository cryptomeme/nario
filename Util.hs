module Util where

-- Utility functions

-- Make pair
pair a b = (a, b)

-- |Replace i-th element of list to v.
replace :: [a] -> Int -> a -> [a]
replace ls i v = take i ls ++ [v] ++ drop  (i + 1) ls

-- Get last n elements
lastN n xs = supply n [] xs
	where
		supply _ acc [] = acc
		supply 0 acc xs = queue acc xs
		supply n acc (x:xs) = supply (n-1) (acc ++ [x]) xs
		queue acc [] = acc
		queue acc (x:xs) = queue (tail acc ++ [x]) xs

-- Get sign of number
sgn x
	| x > 0		= 1
	| x < 0		= -1
	| otherwise	= 0

-- Get n width string of base 10
deciWide n c = lastN n . (replicate n c ++) . show

-- Add value
rangeadd x d x0 x1
	| d > 0 && x < x1	= min (x + d) x1
	| d < 0 && x > x0	= max (x + d) x0
	| otherwise	= x

-- Value goes to 0
friction x d
	| x > d		= x - d
	| x < -d	= x + d
	| otherwise	= 0
