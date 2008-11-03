module Const where

-- Window
wndTitle = "NARIO in Haskell"
screenWidth = 256 :: Int
screenHeight = 224 :: Int
wndBpp = 32 :: Int

frameRate = 60 :: Int

-- Game timer
timeBase = 22 :: Int


-- One for fixed point integer
one = 256 :: Int

-- Size of charcter
chrSize = 16 :: Int

-- Gravity
gravity = one * 2 `div` 5

-- Maximum speed for falling
maxVy = one * 5


-- Score point
pointKuribo		= 100	:: Int
pointNokonoko	= 100	:: Int
pointKinoko		= 1000	:: Int
pointFlower		= 1000	:: Int
pointBreakBlock	= 50	:: Int
pointGetCoin	= 200	:: Int
pointKoura		= 400	:: Int
