
module Sound where

import Multimedia.SDL

{-
  wav <- loadWAV "snd/jump.wav"
-}


-- –Â‚ç‚È‚¢c
playAudioData ad = do
	case audioSpec ad of
		Just spec -> do
			let freq = asFreq spec
			let format = asFormat spec
			let channel = asChannels spec
			let samples = asSamples spec
			print (unlines [show freq, show format, show channel, show samples])
			openAudio freq format channel samples cb
			return ()
		Nothing -> do
			print "audioSpec error"
			return ()
	where
		cb x = return [ad]
