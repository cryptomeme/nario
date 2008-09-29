
module Event where

import Const
import Field

data Event =
		EvHitBlock ImageType Int Int
	|	EvSetField Int Int Cell
