-- スコアが増えるときの増分表示

module Actor.ScoreAdd (
	newScoreAdd
) where

import Multimedia.SDL hiding (Event)

import Actor (Actor(..))
import AppUtil
import Const
import Images

vy = -1

data ScoreAdd = ScoreAdd {
	imgtype :: ImageType,
	sx :: Int,
	sy :: Int,
	cnt :: Int
	}


instance Actor ScoreAdd where
	update _ self = (self { sy = sy self + vy, cnt = cnt self + 1 }, [])

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres $ imgtype self) Nothing sur (pt (sx self - scrx) (sy self))
		return ()

	bDead self = cnt self >= frameRate `div` 2

newScoreAdd :: Int -> Int -> ImageType -> ScoreAdd
newScoreAdd sx' sy' imgtype' =
	ScoreAdd { imgtype = imgtype', sx = sx', sy = sy', cnt = 0 }
