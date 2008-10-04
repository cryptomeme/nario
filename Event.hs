module Event where

import Const
import Images
import Field

data Event =
		-- ブロックをたたいた
		EvHitBlock ImageType Int Int

		-- フィールドを変更
	|	EvSetField Int Int Cell

		-- 敵登場
	|	EvAppearEnemy Int Int Cell
