
-- プレーヤー

module Player (
	Player(..),
	newPlayer,
	updatePlayer,
	renderPlayer
) where

import Multimedia.SDL

import Util
import SDLUtil
import Const


data Player = Player {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	stand :: Bool,

	lr :: Int,
	pat :: Int,
	anm :: Int
	}

newPlayer = Player {
	x = 1 * chrSize * one,
	y = 13 * chrSize * one - 1,
	vx = 0,
	vy = 0,
	stand = True,

	lr = 1,
	pat = 0,
	anm = 0
	}

maxVx = one * 3
acc = one `div` 6


patStop = 0
patWalk = 1
walkPatNum = 3
patJump = patWalk + walkPatNum

imgTable = [
	[ImgNario00, ImgNario01, ImgNario02, ImgNario03, ImgNario04],
	[ImgNario10, ImgNario11, ImgNario12, ImgNario13, ImgNario14]
	]


-- 横移動
moveLR :: KeyProc -> Player -> Player
moveLR kp player =
	player { x = x', vx = vx', lr = lr', pat = pat', anm = anm' }
	where
		ax = (-padl + padr) * acc
		vx'
			| ax /= 0	= rangeadd (vx player) ax (-maxspd) maxspd
			| otherwise	= friction (vx player) acc
		x' = (x player) + vx'
		padl = if isPressed (kp PadL) then 1 else 0
		padr = if isPressed (kp PadR) then 1 else 0
		maxspd
			| isPressed (kp PadB)	= maxVx * 2
			| otherwise				= maxVx

		lr' =
			case (-padl + padr) of
				0	-> lr player
				-1	-> 0
				1	-> 1
		pat'
			| vx' == 0		= patStop
			| otherwise		= (anm' `div` anmCnt) + patWalk
		anm'
			| vx' == 0		= 0
			| otherwise		= ((anm player) + (abs vx')) `mod` (walkPatNum * anmCnt)
		anmCnt = maxVx * 3


-- 縦移動
jumpOrFall :: KeyProc -> Player -> Player
jumpOrFall kp player =
	player { y = y' }
	where
		y'
			| isPressed (kp PadU)	= (y player) - 1 * one
			| isPressed (kp PadD)	= (y player) + 1 * one
			| otherwise				= y player


updatePlayer :: KeyProc -> Player -> Player
updatePlayer kp =
	jumpOrFall kp . moveLR kp


renderPlayer sur player imgres = do
	blitSurface (getImageSurface imgres imgtype) Nothing sur pos
	where
		pos = pt ((x player) `div` one) ((y player) `div` one - chrSize)
		imgtype = imgTable !! (lr player) !! (pat player)
