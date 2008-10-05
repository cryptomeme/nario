-- -*- mode: haskell; Encoding: UTF-8 -*-
-- 甲羅

module Actor.Koura (
	newKoura
) where

import Multimedia.SDL (blitSurface, pt)

import Actor (Actor(..), ActorWrapper(..))
import Actor.Common (updateActorBase, stamp)
import Const
import AppUtil (getImageSurface, Rect(..))
import Images
import Player (getPlayerX, stampPlayer, setPlayerDamage, addScore)
import Event (Event(..))

ofsH = 15
vel = 7 * one `div` 2
reviveCount = 6 * frameRate


data Koura = Koura {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	nomovecnt :: Int
	}

instance Actor Koura where
	update fld self =
		if not revive
			then (self', [])
			else (gotoDie, [])
		where
			self' = self { x = x', y = y', vx = vx', vy = vy', nomovecnt = nomovecnt' }
			(x', y', vx', vy') = updateActorBase fld (x self, y self, vx self, vy self)
			nomovecnt'
				| vx self == 0	= nomovecnt self + 1
				| otherwise		= 0
			revive = nomovecnt' >= reviveCount
			gotoDie = self { x = -chrSize * one }

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres imgtype) Nothing sur (pt ((x self) `div` one - chrSize `div` 2 - scrx) ((y self) `div` one - ofsH - 8))
		return ()
		where
			imgtype = ImgKoura

	bDead self = y self >= (screenHeight + chrSize * 3) * one || x self <= -chrSize * one

	getHitRect self = Just $ Rect (xx - 8) (yy - 16) (xx + 8) yy
		where
			xx = x self `div` one
			yy = y self `div` one

	onHit pl self
		| vx self == 0				= (addScore pointKoura $ pl, Just $ ActorWrapper $ self { vx = (if x self > getPlayerX pl then 1 else -1) * vel }, ev)
		| stamp pl (x self, y self)	= (stampPlayer pl, Just $ ActorWrapper $ self { vx = 0 }, [])
		| collide					= (setPlayerDamage pl, Just $ ActorWrapper self, [])
		| otherwise					= (pl, Just $ ActorWrapper self, [])
		where
			collide = (getPlayerX pl - x self) * (vx self) > 0
			ev = [EvScoreAddEfe (x self `div` one) (y self `div` one - chrSize * 2) pointKoura]


newKoura :: Int -> Int -> Koura
newKoura xx yy =
	Koura { x = xx, y = yy, vx = 0, vy = 0, nomovecnt = 0 }
