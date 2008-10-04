-- フラワー

module Actor.Flower (
	newFlower
) where

import Multimedia.SDL (blitSurface, pt)

import Actor (Actor(..))
import Const
import Util (sgn)
import AppUtil (getImageSurface, cellCrd, Rect(..))
import Images
import Field
import Player (PlayerType(..), getPlayerType, setPlayerType)

maxVy = one * 6


data Flower = Flower {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int
	}

instance Actor Flower where
	update fld self = (self, [])

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres ImgFlower) Nothing sur (pt ((x self) `div` one - chrSize `div` 2 - scrx) ((y self) `div` one - 15 - 8))
		return ()

	getHitRect self = Just $ Rect (xx - 8) (yy - 16) (xx + 8) yy
		where
			xx = x self `div` one
			yy = y self `div` one

	onHit pl self = (setPlayerType nt pl, Nothing)
		where
			nt = case typ of
				SmallNario	-> SuperNario
				SuperNario	-> FireNario
				otherwise	-> typ
			typ = getPlayerType pl


newFlower :: Int -> Int -> Flower
newFlower cx cy =
	Flower { x = cx * chrSize * one + chrSize * one `div` 2, y = cy * chrSize * one, vx = one, vy = 0 }
