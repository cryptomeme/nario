-- クリボー

module Actor.Nokonoko (
	newNokonoko
) where

import Multimedia.SDL hiding (Event)

import Actor (Actor(..))
import Const
import AppUtil
import Images


data Nokonoko = Nokonoko {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	cnt :: Int
	}

instance Actor Nokonoko where
	update fld self = (self { x = x self + vx self, cnt = cnt self + 1 }, [])

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres imgtype) Nothing sur (pt ((x self) `div` one - scrx) ((y self) `div` one - 8))
		return ()
		where
			imgtype = [ImgNoko0, ImgNoko1] !! (cnt self `mod` 16 `div` 8)

newNokonoko :: Int -> Int -> Nokonoko
newNokonoko cx cy =
	Nokonoko { x = cx * chrSize * one, y = cy * chrSize * one, vx = -one `div` 2, vy = 0, cnt = 0 }
