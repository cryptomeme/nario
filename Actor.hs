
module Actor where

import Multimedia.SDL hiding (Event)

import Const
import Util
import Event


data AnimBlock = AnimBlock { startcy :: Int, x :: Int, y :: Int, vy :: Int }

data Actor = ActNull | ActAnimBlock AnimBlock

-- ============================================================================
-- ActNull
--	死亡

updateNull = (ActNull, [])

renderNull imgres scrx sur = return ()

bDieNull = True


-- ============================================================================
-- AnimBlock
--	ブロックを叩いたときのバウンド演出

updateAnimBlock self = result'
	where
		result'
			| not bEnd	= (ActAnimBlock $ self { vy = vy', y = y' }, [])
			| otherwise	= (ActNull, [EvSetField (cellCrd $ x self) (startcy self) '@'])

		vy' = vy self + gravity
		y' = y self + vy'
		bEnd = y' >= startcy self * chrSize * one


renderAnimBlock self imgres scrx sur = do
	blitSurface (getImageSurface imgres ImgBlock2) Nothing sur (pt ((x self) `div` one - scrx) ((y self) `div` one - 8))
	return ()

bDieAnimBlock self = False

newAnimBlock cx cy = ActAnimBlock $ AnimBlock { startcy = cy, x = cx * chrSize * one, y = cy * chrSize * one, vy = -3 * one }




-- ============================================================================

updateActor :: Actor -> (Actor, [Event])
updateActor ActNull				= updateNull
updateActor (ActAnimBlock a)	= updateAnimBlock a

renderActor :: Actor -> ImageResource -> Int -> Surface -> IO ()
renderActor ActNull				= renderNull
renderActor (ActAnimBlock a)	= renderAnimBlock a

bDieActor :: Actor -> Bool
bDieActor ActNull				= bDieNull
bDieActor (ActAnimBlock a)		= bDieAnimBlock a

