{-# OPTIONS_GHC -fglasgow-exts #-}

module Actor where

import Multimedia.SDL hiding (Event)

import Const
import Images
import Util
import AppUtil
import Event

class Actor a where
	update :: a -> (a, [Event])
	render :: a -> ImageResource -> Int -> Surface -> IO ()
	bDead :: a -> Bool

-- ============================================================================
-- ActNull
--	死亡

data AnimNull = AnimNull

instance Actor AnimNull where
	update self = (self, [])
	render self imgres scrx sur = return ()
	bDead self = True


-- ============================================================================
-- AnimBlock
--	ブロックを叩いたときのバウンド演出

data AnimBlock = AnimBlock { startcy :: Int, x :: Int, y :: Int, vy :: Int }

instance Actor AnimBlock where
	update self = result'
		where
			result'
				| not bEnd	= (self { vy = vy', y = y' }, [])
				| otherwise	= (self, [EvSetField (cellCrd $ x self) (startcy self) '@'])

			vy' = vy self + gravity
			y' = y self + vy'
			bEnd = y' >= startcy self * chrSize * one

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres ImgBlock2) Nothing sur (pt ((x self) `div` one - scrx) ((y self) `div` one - 8))
		return ()

	bDead self = False


newAnimBlock cx cy = AnimBlock { startcy = cy, x = cx * chrSize * one, y = cy * chrSize * one, vy = -3 * one }



-- ============================================================================

{-
updateActor :: Actor -> (Actor, [Event])
updateActor ActNull				= updateNull
updateActor (ActAnimBlock a)	= updateAnimBlock a

renderActor :: Actor -> ImageResource -> Int -> Surface -> IO ()
renderActor ActNull				= renderNull
renderActor (ActAnimBlock a)	= renderAnimBlock a

bDieActor :: Actor -> Bool
bDieActor ActNull				= bDieNull
bDieActor (ActAnimBlock a)		= bDieAnimBlock a
-}
