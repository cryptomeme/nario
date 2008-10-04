-- クリボー

module Actor.Kuribo (
	newKuribo
) where

import Multimedia.SDL (blitSurface, pt)

import Actor (Actor(..), ActorWrapper(..))
import Const
import AppUtil
import Images
import Player (setPlayerDamage, getPlayerVY, stampPlayer)


data Kuribo = Kuribo {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	cnt :: Int
	}

instance Actor Kuribo where
	update fld self = (self { x = x self + vx self, cnt = cnt self + 1 }, [])

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres imgtype) Nothing sur (pt (x self `div` one - chrSize `div` 2 - scrx) (y self `div` one - 15 - 8))
		return ()
		where
			imgtype = [ImgKuri0, ImgKuri1] !! (cnt self `mod` 16 `div` 8)

	getHitRect self = Just $ Rect (xx - 8) (yy - 16) (xx + 8) yy
		where
			xx = x self `div` one
			yy = y self `div` one

	onHit pl self
		| stamp		= (stampPlayer pl, Just $ ActorWrapper $ newStampedKuribo (x self `div` one - chrSize `div` 2) (y self `div` one))
		| otherwise	= (setPlayerDamage pl, Just $ ActorWrapper self)
		where
			stamp = getPlayerVY pl > 0

newKuribo :: Int -> Int -> Kuribo
newKuribo cx cy =
	Kuribo { x = cx * chrSize * one + chrSize * one `div` 2, y = (cy+1) * chrSize * one, vx = -one `div` 2, vy = 0, cnt = 0 }


-- 踏みつけられたクリボー
data StampedKuribo = StampedKuribo {
	sx :: Int,
	sy :: Int,
	ccnt :: Int
	}

instance Actor StampedKuribo where
	update fld self = (self { ccnt = ccnt self + 1 }, [])

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres ImgKuriDead) Nothing sur (pt (sx self - scrx) (sy self - 7 - 8))
		return ()

	bDead self = ccnt self >= frameRate `div` 2

newStampedKuribo sx' sy' = StampedKuribo { sx = sx', sy = sy', ccnt = 0 }
