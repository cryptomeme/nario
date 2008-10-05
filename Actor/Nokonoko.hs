-- -*- mode: haskell; Encoding: UTF-8 -*-
-- ノコノコ

module Actor.Nokonoko (
	newNokonoko
) where

import Multimedia.SDL (blitSurface, pt)

import Actor (Actor(..), ActorWrapper(..))
import Actor.Common (updateActorBase, stamp)
import Actor.Koura
import Const
import AppUtil (getImageSurface, cellCrd, Rect(..))
import Images
import Player (setPlayerDamage, stampPlayer, addScore)
import Event (Event(..))

ofsH = 23

data Nokonoko = Nokonoko {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	cnt :: Int
	}

instance Actor Nokonoko where
	update fld self = (self', [])
		where
			self' = self { x = x', y = y', vx = vx', vy = vy', cnt = cnt self + 1 }
			(x', y', vx', vy') = updateActorBase fld (x self, y self, vx self, vy self)

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres imgtype) Nothing sur (pt (x self `div` one - chrSize `div` 2 - scrx) (y self `div` one - ofsH - 8))
		return ()
		where
			imgtype = [ImgNoko0, ImgNoko1] !! (cnt self `mod` 16 `div` 8)

	bDead self = y self >= (screenHeight + chrSize * 3) * one || x self <= -chrSize * one

	getHitRect self = Just $ Rect (xx - 8) (yy - 16) (xx + 8) yy
		where
			xx = x self `div` one
			yy = y self `div` one

	onHit pl self
		| stamp pl (x self, y self)	= (addScore pointNokonoko $ stampPlayer pl, Just $ ActorWrapper $ newKoura (x self) (y self), ev)
		| otherwise	= (setPlayerDamage pl, Just $ ActorWrapper self, [])
		where
			ev = [EvScoreAddEfe (x self `div` one) (y self `div` one - chrSize * 2) pointNokonoko]

newNokonoko :: Int -> Int -> Nokonoko
newNokonoko cx cy =
	Nokonoko { x = cx * chrSize * one, y = cy * chrSize * one, vx = -one `div` 2, vy = 0, cnt = 0 }
