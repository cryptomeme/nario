module Event where

import Images (ImageType)
import Field (Cell)
import {-# SOURCE #-} Actor (Actor(..), ActorWrapper(..))
import Mixer

data Event =
		-- Hit block: x y super?
		EvHitBlock ImageType Int Int Bool

		-- Modify field
	|	EvSetField Int Int Cell

		-- Add actor
	|	EvAddActor ActorWrapper

		-- Add score
	|	EvScoreAddEfe Int Int Int

		-- Play SE
	|	EvSound SoundType
