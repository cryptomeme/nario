{-# OPTIONS_GHC -fglasgow-exts #-}

module Actor (
	Actor (..),
	ObjWrapper (..),
	updateActors,
	filterActors,
	renderActors
) where

import Multimedia.SDL (Surface)

import Event
import AppUtil


class Actor a where
	update :: a -> (a, [Event])
	render :: a -> ImageResource -> Int -> Surface -> IO ()
	bDead :: a -> Bool


-- ============================================================================

----
data ObjWrapper = forall a. Actor a => ObjWrapper a	-- 存在型aの動く範囲を型クラスに制限

updateActors :: [ObjWrapper] -> [(ObjWrapper, [Event])]
updateActors = map (\(ObjWrapper x) -> let (x', ev') = update x in (ObjWrapper x', ev'))

filterActors :: [ObjWrapper] -> [ObjWrapper]
filterActors = filter (\(ObjWrapper x) -> not $ bDead x)

renderActors :: ImageResource -> Int -> Surface -> [ObjWrapper] -> IO ()
renderActors imgres ofsx sur = mapM_ (\(ObjWrapper x) -> render x imgres ofsx sur)
