module Util where

-- ユーティリティ関数

-- ペアを作る
pair a b = (a, b)

-- |Replace i-th element of list to v.
replace :: [a] -> Int -> a -> [a]
replace ls i v = take i ls ++ [v] ++ drop  (i + 1) ls

-- けつの n 個取り出し
lastN n xs = supply n [] xs
	where
		supply _ acc [] = acc
		supply 0 acc xs = queue acc xs
		supply n acc (x:xs) = supply (n-1) (acc ++ [x]) xs
		queue acc [] = acc
		queue acc (x:xs) = queue (tail acc ++ [x]) xs

-- 数値の符号を返す
sgn x
	| x > 0		= 1
	| x < 0		= -1
	| otherwise	= 0

-- １０進数 n 桁文字列を返す
deciWide n c = lastN n . (replicate n c ++) . show

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
