-- -*- mode: haskell; Encoding: UTF-8 -*-
-- スコアが増えるときの増分表示

module Actor.ScoreAdd (
	newScoreAdd
) where

--import Multimedia.SDL hiding (Event)

import Actor (Actor(..))
import AppUtil (putimg)
import Const
import Images

vy = -1

data ScoreAdd = ScoreAdd {
	pnt :: Int,
	sx :: Int,
	sy :: Int,
	cnt :: Int
	}


instance Actor ScoreAdd where
	update _ self = (self { sy = sy self + vy, cnt = cnt self + 1 }, [])

	render self imgres scrx sur =
		putimg sur imgres imgtype (sx self - scrx) (sy self)
		where
			imgtype = case pnt self of
				100		-> Img100
				200		-> Img200
				400		-> Img400
				500		-> Img500
				1000	-> Img1000

	bDead self = cnt self >= frameRate `div` 2

newScoreAdd :: Int -> Int -> Int -> ScoreAdd
newScoreAdd sx' sy' pnt' =
	ScoreAdd { pnt = pnt', sx = sx', sy = sy', cnt = 0 }
