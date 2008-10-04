{-# OPTIONS_GHC -fglasgow-exts #-}

module Actor (
	Actor(..),
	ObjWrapper(..),
	updateActors,
	filterActors,
	renderActors
) where

import Multimedia.SDL (Surface)

import Event
import AppUtil
import Field 


class Actor a where
	update :: Field -> a -> (a, [Event])
	render :: a -> ImageResource -> Int -> Surface -> IO ()
	bDead :: a -> Bool
	bDead _ = False

-- ============================================================================

----
data ObjWrapper = forall a. Actor a => ObjWrapper a	-- 存在型aの動く範囲を型クラスに制限

updateActors :: Field -> [ObjWrapper] -> [(ObjWrapper, [Event])]
updateActors fld = map (\(ObjWrapper x) -> let (x', ev') = update fld x in (ObjWrapper x', ev'))

filterActors :: [ObjWrapper] -> [ObjWrapper]
filterActors = filter (\(ObjWrapper x) -> not $ bDead x)

renderActors :: ImageResource -> Int -> Surface -> [ObjWrapper] -> IO ()
renderActors imgres ofsx sur = mapM_ (\(ObjWrapper x) -> render x imgres ofsx sur)
