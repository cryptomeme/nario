module Event where

import Images (ImageType)
import Field (Cell)
import {-# SOURCE #-} Actor (Actor(..), ActorWrapper(..))
import Mixer

data Event =
		-- ブロックをたたいた x y super?
		EvHitBlock ImageType Int Int Bool

		-- フィールドを変更
	|	EvSetField Int Int Cell

		-- アクター追加
	|	EvAddActor ActorWrapper

		-- スコア加算エフェクト
	|	EvScoreAddEfe Int Int Int

		-- サウンドを鳴らす
	|	EvSound SoundType
