
module Event where

import Const
import Images
import Field

data Event =
		EvHitBlock ImageType Int Int
	|	EvSetField Int Int Cell
	|	EvAppearEnemy Int Int Cell
