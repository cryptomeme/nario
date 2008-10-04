{-# OPTIONS_GHC -fglasgow-exts #-}

module Actor (
	Actor(..),
	ActorWrapper(..),
	updateActors,
	filterActors,
	renderActors
) where

import Multimedia.SDL (Surface)

import Event (Event)
import AppUtil (ImageResource, Rect)
import Field (Field)


class Actor a where
	update :: Field -> a -> (a, [Event])
	render :: a -> ImageResource -> Int -> Surface -> IO ()

	bDead :: a -> Bool
	bDead _ = False

	getHitRect :: a -> Maybe Rect
	getHitRect _ = Nothing

-- ============================================================================

----
data ActorWrapper = forall a. Actor a => ActorWrapper a	-- 存在型aの動く範囲を型クラスに制限

updateActors :: Field -> [ActorWrapper] -> [(ActorWrapper, [Event])]
updateActors fld = map (\(ActorWrapper x) -> let (x', ev') = update fld x in (ActorWrapper x', ev'))

filterActors :: [ActorWrapper] -> [ActorWrapper]
filterActors = filter (\(ActorWrapper x) -> not $ bDead x)

renderActors :: ImageResource -> Int -> Surface -> [ActorWrapper] -> IO ()
renderActors imgres ofsx sur = mapM_ (\(ActorWrapper x) -> render x imgres ofsx sur)
