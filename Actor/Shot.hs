-- -*- mode: haskell; Encoding: UTF-8 -*-
-- Shot (fire)

module Actor.Shot (
	newShot
) where

import Actor (Actor(..))
import AppUtil (cellCrd, putimg)
import Const
import Images
import Field (Cell, isBlock, fieldRef)
import Event (Event(..))

velX = 4 * one
velY = 3 * one
size = 8


data Shot = Shot {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	cnt :: Int,
	dead :: Bool
	}

instance Actor Shot where
	update fld self = (self { x = x', y = y', vy = vy', cnt = cnt', dead = dead' }, [])
		where
			vytmp = min velY $ vy self + gravity
			vy'
				| isFloor	= -vytmp
				| otherwise	= vytmp
			isFloor = isBlock $ fieldRef fld (cellCrd $ x self) (cellCrd $ y self + vytmp)
			y' = y self + vy'

			x' = x self + vx self
			cnt' = cnt self + 1

			dead' = isBlock $ fieldRef fld (cellCrd x') (cellCrd y')

	render self imgres scrx sur =
		putimg sur imgres imgtype (x self `div` one - size `div` 2 - scrx) (y self `div` one - size `div` 2 - 8)
		where
			imgtype = [ImgFire0, ImgFire1, ImgFire2, ImgFire3] !! (cnt self `mod` 4)

	bDead self = dead self || x self < -size * one || y self >= (screenHeight + size) * one


newShot :: Int -> Int -> Int -> Shot
newShot xx yy lr =
	Shot { x = x', y = y', vx = vx', vy = vy', cnt = 0, dead = False }
	where
		dir = if lr == 0 then -1 else 1
		x' = xx + dir * (8 * one)
		y' = yy - 16 * one
		vx' = dir * velX
		vy' = velY
