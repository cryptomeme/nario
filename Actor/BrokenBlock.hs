-- -*- mode: haskell; Encoding: UTF-8 -*-
-- ブロックの破片

module Actor.BrokenBlock (
	newBrokenBlock
) where

import Actor (Actor(..))
import Const
import AppUtil (putimg)
import Images


data BrokenBlock = BrokenBlock {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int
	}

instance Actor BrokenBlock where
	update _ self = (self { x = x self + vx self, y = y self + vy self, vy = vy self + gravity }, [])

	render self imgres scrx sur =
		putimg sur imgres ImgBroken (x self `div` one - 4 - scrx) (y self `div` one - 4 - 8)

	bDead self = y self >= (screenHeight + chrSize * 2) * one

newBrokenBlock :: Int -> Int -> [BrokenBlock]
newBrokenBlock cx cy =
	[	BrokenBlock { x = xx + dx1, y = yy + dy1, vx = -vx', vy = vy1 },
		BrokenBlock { x = xx + dx2, y = yy + dy1, vx =  vx', vy = vy1 },
		BrokenBlock { x = xx + dx1, y = yy + dy2, vx = -vx', vy = vy2 },
		BrokenBlock { x = xx + dx2, y = yy + dy2, vx =  vx', vy = vy2 }	]
	where
		xx = cx * chrSize * one
		yy = cy * chrSize * one
		dx1 =  4 * one
		dx2 = 12 * one
		dy1 =  4 * one
		dy2 = 12 * one
		vx' =  3 * one `div` 2
		vy1 = -8 * one
		vy2 = -5 * one
