module Const where

-- ウィンドウ周り
wndTitle = "NARIO in Haskell"
screenWidth = 256 :: Int
screenHeight = 224 :: Int
wndBpp = 32 :: Int

frameRate = 60 :: Int


-- 固定小数点での１
one = 256 :: Int

-- １キャラのサイズ
chrSize = 16 :: Int

-- 重力
gravity = one `div` 2
