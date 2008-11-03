{-# LANGUAGE ExistentialQuantification #-}
-- -*- mode: haskell; Encoding: UTF-8 -*-

-- Object that appear in the game

module Actor (
	Actor(..),
	ActorWrapper(..),
	updateActors,
	filterActors,
	renderActors
) where

import Graphics.UI.SDL (Surface)

import AppUtil (ImageResource, Rect)
import Event (Event)
import Field (Field)
import {-# SOURCE #-} Player (Player)


class Actor a where
	update :: Field -> a -> (a, [Event])
	render :: a -> ImageResource -> Int -> Surface -> IO ()

	bDead :: a -> Bool
	bDead _ = False

	getHitRect :: a -> Maybe Rect
	getHitRect _ = Nothing

	onHit :: Player -> a -> (Player, Maybe ActorWrapper, [Event])
	onHit pl ac = (pl, Nothing, [])


----
data ActorWrapper = forall a. Actor a => ActorWrapper a	-- 存在型aの動く範囲を型クラスに制限

updateActors :: Field -> [ActorWrapper] -> [(ActorWrapper, [Event])]
updateActors fld = map (\(ActorWrapper x) -> let (x', ev') = update fld x in (ActorWrapper x', ev'))

filterActors :: [ActorWrapper] -> [ActorWrapper]
filterActors = filter (\(ActorWrapper x) -> not $ bDead x)

renderActors :: ImageResource -> Int -> Surface -> [ActorWrapper] -> IO ()
renderActors imgres ofsx sur = mapM_ (\(ActorWrapper x) -> render x imgres ofsx sur)
