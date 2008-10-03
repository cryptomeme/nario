{-# OPTIONS_GHC -fglasgow-exts #-}

module Actor where

import Multimedia.SDL hiding (Event)

import Const
import Images
import Util
import AppUtil
import Event
import Field

class Actor a where
	update :: a -> (a, [Event])
	render :: a -> ImageResource -> Int -> Surface -> IO ()
	bDead :: a -> Bool


-- ============================================================================
-- AnimBlock
--	ブロックを叩いたときのバウンド演出

data AnimBlock = AnimBlock {
	startcy :: Int,
	x :: Int,
	y :: Int,
	vy :: Int,
	chr :: Cell
	}

instance Actor AnimBlock where
	update self
		| not (bDead self)	= (self', ev')
		| otherwise			= (self, [])

		where
			vy' = vy self + gravity
			y' = y self + vy'
			self' = self { vy = vy', y = y' }
			ev' = if (bDead self')
				then [EvSetField (cellCrd $ x self) (startcy self) $ chr self]
				else []

	render self imgres scrx sur = do
		blitSurface (getImageSurface imgres $ chr2img $ chr self) Nothing sur (pt ((x self) `div` one - scrx) ((y self) `div` one - 8))
		return ()

	bDead self = vy self > 0 && y self >= startcy self * chrSize * one

newAnimBlock :: Int -> Int -> Cell -> AnimBlock
newAnimBlock cx cy c =
	AnimBlock { startcy = cy, x = cx * chrSize * one, y = cy * chrSize * one, vy = -3 * one, chr = cc }
	where
		cc = case c of
			'?'	-> '#'
			x	-> x


-- ============================================================================

----
data ObjWrapper = forall a. Actor a => ObjWrapper a	-- ݌^a͈̓͂잃NXDuckɐ

updateActors :: [ObjWrapper] -> [(ObjWrapper, [Event])]
updateActors = map (\(ObjWrapper x) -> let (x', ev') = update x in (ObjWrapper x', ev'))

filterActors :: [ObjWrapper] -> [ObjWrapper]
filterActors = filter (\(ObjWrapper x) -> not $ bDead x)

renderActors :: ImageResource -> Int -> Surface -> [ObjWrapper] -> IO ()
renderActors imgres ofsx sur = mapM_ (\(ObjWrapper x) -> render x imgres ofsx sur)
