-- -*- mode: haskell; Encoding: UTF-8 -*-
-- Common action

module Actor.Common (
	updateActorBase,
	stamp
) where

import Const
import Field
import Util (sgn)
import AppUtil (cellCrd)
import Player (Player(..), getPlayerVY)


-- 共通動作
{-
	左右に移動、壁にぶつかったら反転、下に何もなかったら落下
-}
updateActorBase :: Field -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
updateActorBase fld (x, y, vx, vy)
	| isGround	= (x', groundy', vx', 0)
	| otherwise	= (x', y', vx', vy')
	where
		x' = x + vx
		sideWall = isBlock $ fieldRef fld (cellCrd $ x' + sgn vx * 6 * one) (cellCrd $ y - chrSize * one `div` 2)
		vx'
			| sideWall	= -vx
			| otherwise	= vx

		vy' = min maxVy $ vy + gravity
		y' = y + vy'
		isGround = isBlock $ fieldRef fld (cellCrd $ x') (cellCrd y')
		groundy' = (cellCrd y') * one * chrSize


-- プレーヤーに踏みつけられた？
stamp :: Player -> (Int, Int) -> Bool
stamp pl (x, y) = getPlayerVY pl > 0
