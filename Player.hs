module Player where

import Multimedia.SDL

import Util
import SDLUtil
import Const

-- ƒvƒŒ[ƒ„[

data Player = Player {
	x :: Int,
	y :: Int,
	lr :: Int
	}

newPlayer = Player {
	x = 1 * 16 * one,
	y = 12 * 16 * one,
	lr = 1
	}

updatePlayer :: Player -> KeyProc -> Player
updatePlayer player kp =
	player { x = x', y = y', lr = lr' }
	where
		x'
			| isPressed (kp PadL)	= (x player) - 1 * one
			| isPressed (kp PadR)	= (x player) + 1 * one
			| otherwise				= x player
		y'
			| isPressed (kp PadU)	= (y player) - 1 * one
			| isPressed (kp PadD)	= (y player) + 1 * one
			| otherwise				= y player
		lr'
			| isPressed (kp PadL)	= 0
			| isPressed (kp PadR)	= 1
			| otherwise					= lr player

renderPlayer sur player imgres =
	blitSurface (getImageSurface imgres chr) Nothing sur pos
	where
		pos = pt ((x player) `div` one) ((y player) `div` one)
		chr = if (lr player) == 0
				then ImgNario00
				else ImgNario10
