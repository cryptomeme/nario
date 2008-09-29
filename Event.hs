
module Event where

import Const

data Event =
		EvHitBlock ImageType Int Int
	|	EvSetField Int Int Char
