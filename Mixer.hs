module Mixer where

import Graphics.UI.SDL
import Data.IORef
import Data.Maybe (catMaybes)
import Util (pair)

type AudioData = ()
type Mixer = IORef [AudioData]

--freq = 22050
--format = AUDIO_S16LSB
--channel = 1
--samples = 4096

createMixer = do
	mixer <- newIORef []
--	openAudio freq format channel samples (cb mixer)
	return mixer
--	where
--		cb mixer len = do
--			wavs <- readIORef mixer
--			lockAudio
--			writeIORef mixer $ filter (not . audioIsEnd) $ map (audioAdvance len) wavs
--			unlockAudio
--			return wavs

playWav mixer Nothing = return ()
--playWav mixer (Just ad) = do
--	pauseAudio 1
--	lockAudio
--	modifyIORef mixer (++ [ad])
--	unlockAudio
--	pauseAudio 0
--	return ()
playWav mixer _ = return ()


data SoundType =
		SndJump
	|	SndShot
	|	SndPunch
	deriving (Eq, Show)

soundFn SndJump = "jump.wav"
soundFn SndShot = "jump.wav"
soundFn SndPunch = "jump.wav"

soundTypes = [SndJump, SndShot, SndPunch]

type SoundResource = [(SoundType, AudioData)]

loadSoundResource :: [SoundType] -> IO SoundResource
--loadSoundResource sndtypes = mapM load sndtypes >>= return . catMaybes
--	where
--		load :: SoundType -> IO (Maybe (SoundType, AudioData))
--		load sndtype = do
--			dat <- loadWAV $ ("data/snd/" ++) $ soundFn sndtype
--			return $ dat >>= Just . pair sndtype
loadSoundResource sndtypes = return []
