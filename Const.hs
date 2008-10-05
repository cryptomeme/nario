module Const where

-- ウィンドウ周り
wndTitle = "NARIO in Haskell"
screenWidth = 256 :: Int
screenHeight = 224 :: Int
wndBpp = 32 :: Int

frameRate = 60 :: Int

-- ゲーム時間の基準
timeBase = 22 :: Int


-- 固定小数点での１
one = 256 :: Int

-- １キャラのサイズ
chrSize = 16 :: Int

-- 重力
gravity = one * 2 `div` 5


-- ポイント
pointKuribo		= 100	:: Int
pointNokonoko	= 100	:: Int
pointKinoko		= 1000	:: Int
pointFlower		= 1000	:: Int
pointBreakBlock	= 50	:: Int
pointGetCoin	= 200	:: Int
pointKoura		= 400	:: Int
