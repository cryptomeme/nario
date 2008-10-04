module Event where

import Const
import Images
import Field

data Event =
		-- ブロックをたたいた x y super?
		EvHitBlock ImageType Int Int Bool

		-- フィールドを変更
	|	EvSetField Int Int Cell

		-- 敵登場
	|	EvAppearEnemy Int Int Cell
