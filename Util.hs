module Util where


-- ユーティリティ関数

-- |Replace i-th element of list to v.
replace :: [a] -> Int -> a -> [a]
replace ls i v = take i ls ++ [v] ++ drop  (i + 1) ls

-- 符号を返す
sgn x
	| x > 0		= 1
	| x < 0		= -1
	| otherwise	= 0

-- x に d を加算した結果が x0～x1 の範囲内を超えないようにする
-- もとから範囲外だったときはそれ以上遠ざからないように
rangeadd x d x0 x1
	| d > 0 && x < x1	= min (x + d) x1
	| d < 0 && x > x0	= max (x + d) x0
	| otherwise	= x


-- 値を０に近づける
friction x d
	| x > d		= x - d
	| x < -d	= x + d
	| otherwise	= 0
