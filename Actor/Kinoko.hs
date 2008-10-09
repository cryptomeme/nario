-- -*- mode: haskell; Encoding: UTF-8 -*-
-- きのこ

module Actor.Kinoko (
	newKinoko
) where

import Actor (Actor(..))
import Actor.Common (updateActorBase)
import AppUtil (Rect(..), putimg)
import Const
import Images
import Player (PlayerType(..), getPlayerType, setPlayerType, addScore)
import Event (Event(..))

ofsH = 15


data Kinoko = Kinoko {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int
	}

instance Actor Kinoko where
	update fld self = (self', [])
		where
			self' = self { x = x', y = y', vx = vx', vy = vy' }
			(x', y', vx', vy') = updateActorBase fld (x self, y self, vx self, vy self)

	render self imgres scrx sur =
		putimg sur imgres ImgKinoko ((x self) `div` one - chrSize `div` 2 - scrx) ((y self) `div` one - ofsH - 8)

	bDead self = y self >= (screenHeight + chrSize * 3) * one || x self <= -chrSize * one

	getHitRect self = Just $ Rect (xx - 8) (yy - 16) (xx + 8) yy
		where
			xx = x self `div` one
			yy = y self `div` one

	onHit pl self = (addScore pointKinoko $ setPlayerType nt pl, Nothing, ev)
		where
			nt = case typ of
				SmallNario	-> SuperNario
				otherwise	-> typ
			typ = getPlayerType pl
			ev = [EvScoreAddEfe (x self `div` one) (y self `div` one - chrSize * 2) pointKinoko]

newKinoko :: Int -> Int -> Kinoko
newKinoko cx cy =
	Kinoko { x = cx * chrSize * one + chrSize * one `div` 2, y = cy * chrSize * one, vx = one, vy = 0 }
