{-# LANGUAGE ForeignFunctionInterface #-}
-- Nario

module Main where

import Graphics.UI.SDL hiding (Event)
import Graphics.UI.SDL.Utilities
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Concurrent (threadDelay)

--import Control.Exception

import Player
import Field
import Util
import AppUtil
import Const
import Images
import Font
import Event
import Actor
import Actor.AnimBlock
import Actor.Kuribo
import Actor.Nokonoko
import Actor.Kinoko
import Actor.Flower
import Actor.BrokenBlock
import Actor.CoinGet
import Actor.ScoreAdd
import Mixer

import Data.List
import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

foreign export ccall "start_hs" main :: IO ()

-- Background color
backColor = Pixel 0x5080FF

-- Display command
type Scr = Surface -> Mixer -> IO ()

foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

getKeyState :: IO [SDLKey]
getKeyState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  numkeys <- peek numkeysPtr
  (map Graphics.UI.SDL.Utilities.toEnum . map fromIntegral . findIndices (== 1)) `fmap` peekArray (fromIntegral numkeys) keysPtr


-- Program etrny point
main :: IO ()
main = do
	Graphics.UI.SDL.init [InitVideo]
	setCaption wndTitle wndTitle
	sur <- setVideoMode screenWidth screenHeight wndBpp [HWSurface, DoubleBuf, AnyFormat]
	do
		mixer <- createMixer
		strm <- delayedStream (1000000 `div` frameRate) fetch
		scrs <- process $ map snd $ takeWhile notQuit strm
		mapM_ (\scr -> scr sur mixer) scrs
	quit

	where
		-- fetch for environment
		fetch = do
			quit <- checkSDLEvent
			ks <- getKeyState
			return (quit, ks)
		notQuit = not . fst

-- Delayed stream
-- return result list of action, interval microsec
delayedStream :: Int -> IO a -> IO [a]
delayedStream microsec func = unsafeInterleaveIO $ do
--	threadDelay microsec	-- Using this cause serious slow down in Ubuntu (bad precision?)
	delay (fromInteger (toInteger (microsec `div` 1000)))		-- SDL.Time.delay
	x <- func
	xs <- delayedStream microsec func
	return $ x:xs

-- Process SDL events
-- return True if quit event has come
checkSDLEvent = do
	ev <- pollEvent
	case ev of
		Quit	-> return True
		KeyDown (Keysym { symKey = ks, symModifiers = km } )
			| ks == SDLK_ESCAPE -> return True
			| ks == SDLK_F4 && (KeyModLeftAlt `elem` km || KeyModRightAlt `elem` km) -> return True
		NoEvent	-> return False
		_		-> checkSDLEvent


-- State of Game
data GameGame =
	GameGame {
		pl :: Player,
		fld :: Field,
		actors :: [ActorWrapper],
		time :: Int,
		snds :: [SoundType]
	}


-- Process whole key input and return display command list
process :: [[SDLKey]] -> IO [Scr]
process kss = do
	imgres <- loadImageResource imageTypes
	sndres <- loadSoundResource soundTypes
	fldmap <- loadField 0

	let tmpscrs = doTitle fldmap kss
	let scrs = zipWith (common imgres sndres) tmpscrs kss
	return $ scrs ++ [final imgres sndres]

	where
		-- Common Action
		common imgres sndres scr ks sur mixer = do
			scr imgres sndres sur mixer
			if SDLK_s `elem` ks
				then saveBMP sur "ss.bmp" >> return ()
				else return ()
			Graphics.UI.SDL.flip sur
			return ()
		-- Finalize
		final imgres sndres sur mixer = releaseImageResource imgres

-- Title
doTitle :: Field -> [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
doTitle fldmap kss = loop kss
	where
		loop :: [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
		loop (ks:kss) = res : left ks kss

		res imgres sndres sur mixer = do
			fillRect sur Nothing backColor
			renderTitle imgres sur

		left ks kss
			| SDLK_SPACE `elem` ks	= doGame fldmap kss
			| otherwise				= loop kss


-- Scroll event
scrollEvent :: Field -> Int -> (Field, [Event])
scrollEvent fld cx
	| cx < length (head fld)	= foldl proc (fld, []) $ zip [0..] cols
	| otherwise					= (fld, [])
	where
		proc (f, e) (cy, c) =
			case event cy c of
				Just ev	-> (fieldSet f cx cy ' ', ev : e)
				Nothing	-> (f, e)
		cols = map (!! cx) fld
		event cy c
			| c `elem` "kn"	= Just $ EvAddActor $ genActor c
			| otherwise		= Nothing
			where
				genActor c = case c of
					'k'	-> ActorWrapper $ newKuribo cx cy
					'n'	-> ActorWrapper $ newNokonoko cx cy



-- Collision detection and response
hitcheck :: Player -> [ActorWrapper] -> (Player, [ActorWrapper], [Event])
hitcheck player actors = foldl proc (player, [], []) actors
	where
		proc (pl, ac, ev) (ActorWrapper a) = case getHitRect a of
			Nothing	-> nothingHappened
			Just rc	->
				if not $ ishit plrc rc
					then nothingHappened
					else (pl', ac', ev')
			where
				nothingHappened = (pl, ac ++ [ActorWrapper a], ev)
				plrc = getPlayerHitRect player
				(pl', a', evtmp) = onHit pl a
				ac' = case a' of
					Just a''	-> ac ++ [a'']
					Nothing		-> ac
				ev' = ev ++ evtmp


-- Game
doGame :: Field -> [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
doGame fldmap kss = loop (head kss) initialState kss
	where
		loop :: [SDLKey] -> GameGame -> [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
		loop bef gs (ks:kss) = scr' : left ks kss
			where
				(scr', gs') = updateProc (keyProc bef ks) gs
				isPlayerDead = getPlayerY (pl gs') >= (screenHeight + chrSize * 2) * one
				timeOver = time gs' <= 0

				left ks kss
					| isPlayerDead || timeOver	= doGameOver fldmap kss
					| otherwise					= loop ks gs' kss

		-- Update
		updateProc :: KeyProc -> GameGame -> (ImageResource -> SoundResource -> Scr, GameGame)
		updateProc kp gs = (scr', gs')
			where
				time' = max 0 (time gs - 1)
				(fld', screv') = scrollEvent (fld gs) $ getScrollPos (pl gs) `div` chrSize + 18

				(pl', plev) = updatePlayer kp fld' (pl gs)
				actors_updates = updateActors (fld gs) (actors gs)
				actors' = filterActors $ map fst actors_updates
				ev' = concatMap snd actors_updates

				(pl'', actors'', ev'') = hitcheck pl' actors'

				gstmp = gs { pl = pl'', fld = fld', actors = actors'', time = time' }
				allEvent = plev ++ ev' ++ screv' ++ ev''
				gs' = procEvent gstmp allEvent
				scr' imgres sndres sur mixer = do
					mapM_ (\ev -> case ev of
							EvSound sndtype	->	play sndtype
							otherwise		->	return ()
						) allEvent
					renderProc gs' imgres sndres sur mixer

					where
						play sndtype = do
							if False
								then
									playWav mixer $ lookup sndtype sndres
								else do
									-- Instead of play wav, print message
									--putStrLn $ "play " ++ show sndtype
									return ()

		initialState = GameGame { pl = newPlayer, fld = fldmap, actors = [], time = 400 * timeBase, snds = [] }


-- Game over
doGameOver fldmap kss = doTitle fldmap kss


-- Process events
procEvent :: GameGame -> [Event] -> GameGame
procEvent gs ev = foldl proc gs ev
	where
		proc gs (EvHitBlock imgtype cx cy bSuper)
			| hardBlock c			= gs
			| bSuper && breakable c	= breakBlock gs cx cy
			| c == 'K'				= genKinoko
			| c == '?'				= getCoin
			| otherwise				= gs'
			where
				c = fieldRef (fld gs) cx cy
				breakable c = c == 'O'

				gs' = gs { fld = fld', actors = actors' }
				actors' = actors gs ++ [ActorWrapper $ newAnimBlock cx cy $ fieldRef (fld gs) cx cy]
				fld' = fieldSet (fld gs) cx cy '*'

				breakBlock gs cx cy =
					gs {
						fld = fieldSet (fld gs) cx cy ' ',
						actors = actors gs ++ map ActorWrapper (newBrokenBlock cx cy),
						pl = addScore pointBreakBlock $ pl gs
						}
				genKinoko = gs' { actors = actors gs' ++ [a] }
					where a = if not bSuper then ActorWrapper $ newKinoko cx cy else ActorWrapper $ newFlower cx cy
				getCoin = gs' { actors = actors gs' ++ [ActorWrapper a], pl = addScore pointGetCoin $ playerGetCoin $ pl gs' }
					where a = newCoinGet cx cy

		proc gs (EvSetField cx cy c) = gs { fld = fieldSet (fld gs) cx cy c }
		proc gs (EvAddActor act) = gs { actors = actors gs ++ [act] }
		proc gs (EvScoreAddEfe sx sy pnt) = gs { actors = actors gs ++ [ActorWrapper $ newScoreAdd sx sy pnt] }
		proc gs (EvSound sndtype) = gs

-- Render
renderProc :: GameGame -> ImageResource -> SoundResource -> Scr
renderProc gs imgres sndres sur mixer = do
	fillRect sur Nothing backColor

	let scrx = getScrollPos (pl gs)

	renderField sur imgres scrx (fld gs)
	renderInfo gs imgres sur
	renderActors imgres scrx sur (actors gs)
	renderPlayer sur imgres scrx (pl gs)

	return ()

-- Render information
renderInfo gs imgres sur = do
	puts  3 1 "NARIO"
	puts  3 2 $ deciWide 6 '0' $ getPlayerScore (pl gs)
	puts 11 2 ("?*" ++ deciWide 2 '0' (getPlayerCoin $ pl gs))
	puts 18 1 "WORLD"
	puts 19 2 "1-1"
	puts 25 1 "TIME"
	puts 26 2 $ deciWide 3 '0' $ (time gs + timeBase-1) `div` timeBase
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16

-- Render title screen
renderTitle imgres sur = do
	putimg sur imgres ImgTitle (5*8) (3*8)
--	puts 13 14 "@1985 NINTENDO"
	puts  9 17 "> 1 PLAYER GAME"
--	puts  9 19 "  2 PLAYER GAME"
	puts 12 22 "TOP- 000000"
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16
