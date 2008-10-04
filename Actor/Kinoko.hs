-- きのこ

module Actor.Kinoko (
	newKinoko
) where

import Multimedia.SDL (blitSurface, pt)

import Actor (Actor(..))
import Const
import Util (sgn)
import AppUtil (getImageSurface, cellCrd, Rect(..))
import Images
import Field

maxVy = one * 6


data Kinoko = Kinoko {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int
	}

instance Actor Kinoko where
	update fld self
		| isGround	= (self { x = x', vx = vx', y = (cellCrd y') * one * chrSize, vy = 0 }, [])
		| otherwise	= (self { x = x', vx = vx', y = y', vy = vy' }, [])
		where
			x' = x self + vx self
			sideWall = isBlock $ fieldRef fld (cellCrd $ x' + sgn (vx self) * 6 * one) (cellCrd $ y self - chrSize * one `div` 2)
			vx'
				| sideWall	= -(vx self)
				| otherwise	= vx self

			vy' = min maxVy $ vy self + gravity
			y' = y self + vy'
			isGround = isBlock $ fieldRef fld (cellCrd $ x') (cellCrd y')

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres imgtype) Nothing sur (pt ((x self) `div` one - chrSize `div` 2 - scrx) ((y self) `div` one - 15 - 8))
		return ()
		where
			imgtype = ImgKinoko

	bDead self = y self `div` one >= screenHeight + chrSize * 3

	getHitRect self = Just $ Rect (xx - 8) (yy - 16) (xx + 8) yy
		where
			xx = x self `div` one
			yy = y self `div` one


newKinoko :: Int -> Int -> Kinoko
newKinoko cx cy =
	Kinoko { x = cx * chrSize * one + chrSize * one `div` 2, y = cy * chrSize * one, vx = one, vy = 0 }
