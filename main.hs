
-- Nario

module Main where

import Multimedia.SDL
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromJust)

import SDLUtil
import Util

-----------------------------------
-- システム周り 
-----------------------------------

-- 種種の定義

wndTitle = "Nario in Haskell"
wndSize  = sz 256 240


-- 画像
data ImageType =
		ImgNario00 | ImgNario01 | ImgNario02 | ImgNario03 | ImgNario04
	|	ImgNario10 | ImgNario11 | ImgNario12 | ImgNario13 | ImgNario14
	|	ImgBlock1 | ImgBlock2 | ImgBlock3 | ImgBlock4 | ImgBlock5
	deriving Eq

imageFn ImgNario00 = "nario00.bmp"
imageFn ImgNario01 = "nario01.bmp"
imageFn ImgNario02 = "nario02.bmp"
imageFn ImgNario03 = "nario03.bmp"
imageFn ImgNario04 = "nario04.bmp"
imageFn ImgNario10 = "nario10.bmp"
imageFn ImgNario11 = "nario11.bmp"
imageFn ImgNario12 = "nario12.bmp"
imageFn ImgNario13 = "nario13.bmp"
imageFn ImgNario14 = "nario14.bmp"
imageFn ImgBlock1 = "block1.bmp"
imageFn ImgBlock2 = "block2.bmp"
imageFn ImgBlock3 = "block3.bmp"
imageFn ImgBlock4 = "block4.bmp"
imageFn ImgBlock5 = "block5.bmp"

images = [
	ImgNario00, ImgNario01, ImgNario02, ImgNario03, ImgNario04,
	ImgNario10, ImgNario11, ImgNario12, ImgNario13, ImgNario14,
	ImgBlock1, ImgBlock2, ImgBlock3, ImgBlock4, ImgBlock5
	]



type ImageResource = [(ImageType, Surface)]




fieldMap = [
	"                ",
	"                ",
	"                ",
	"                ",
	"                ",
	"                ",
	"        O?O     ",
	"                ",
	"                ",
	"       O?O?O    ",
	"                ",
	"                ",
	"                ",
	"@@@@@@@@@@@@@@@@",
	"@@@@@@@@@@@@@@@@"
	]

chr2img '@' = ImgBlock1
chr2img 'O' = ImgBlock2
chr2img '?' = ImgBlock4

renderMap sur imgres = sequence_ $ concatMap lineProc $ zip [0..] fieldMap
	where
		lineProc (y, ln) = map (cellProc y) $ zip [0..] ln
		cellProc y (x, c) = do
			if c == ' '
				then return ()
				else do
					blitSurface (getImageSurface imgres $ chr2img c) Nothing sur $ pt (x*16) (y*16)
					return ()




data Player = Player {
	x :: Int,
	y :: Int,
	lr :: Int
	}

one = 256

newPlayer = Player {
	x = 1 * 16 * one,
	y = 12 * 16 * one,
	lr = 1
	}

updatePlayer :: Player -> KeyProc -> Player
updatePlayer player kp =
	player { x = x', y = y', lr = lr' }
	where
		x'
			| isPressed (kp GKLeft)		= (x player) - 1 * one
			| isPressed (kp GKRight)	= (x player) + 1 * one
			| otherwise					= x player
		y'
			| isPressed (kp GKUp)		= (y player) - 1 * one
			| isPressed (kp GKDown)		= (y player) + 1 * one
			| otherwise					= y player
		lr'
			| isPressed (kp GKLeft)		= 0
			| isPressed (kp GKRight)	= 1
			| otherwise					= lr player

renderPlayer sur player imgres =
	blitSurface (getImageSurface imgres chr) Nothing sur pos
	where
		pos = pt ((x player) `div` one) ((y player) `div` one)
		chr = if (lr player) == 0
				then ImgNario00
				else ImgNario10


-- 画像リソース読み込み
loadImageResource :: IO ImageResource
loadImageResource = mapM load images
	where
		load imgtype = do
			sur <- loadBMP $ ("img/" ++) $ imageFn imgtype
--			colorKey <- mapRGB (surfacePixelFormat sur) $ Color r g b a
			setColorKey sur [SRCCOLORKEY] 0
			return (imgtype, sur)
		r = 0
		g = 0
		b = 0
		a = 255


getImageSurface :: ImageResource -> ImageType -> Surface
getImageSurface imgres t = fromJust $ lookup t imgres

-- メインループ

main :: IO ()
main = sdlStart [VIDEO] wndTitle wndSize $ \sur -> do
	gs <- newIORef initState
	imgres <- loadImageResource

	et <- elapseTime 60
	loop et gs onProcess (onDraw sur imgres) []

loop et gs op od bef = do
	quit <- checkEvent
	when (not quit) $ do
		ks <- getKeyState
		modifyIORef gs $ op $ keyProc bef ks
		st <- readIORef gs
		(fps,draw) <- et
		when draw $ od st
		loop et gs op od ks

checkEvent = do
  ev <- pollEvent
  case ev of
    Just QuitEvent -> return True
    Just (KeyboardEvent { kbPress = True, kbKeysym = Keysym { ksSym = ks, ksMod = km } })
      | ks == SDLK_ESCAPE -> return True
      | ks == SDLK_F4 && (KMOD_LALT `elem` km ||
                          KMOD_RALT `elem` km) -> return True
    Nothing        -> return False
    _              -> checkEvent


sdlStart fs title (Size w h) p = do
	True <- sdlInit fs
	setCaption title title
	sur  <- setVideoMode w h 32 [HWSURFACE,DOUBLEBUF,ANYFORMAT]
	p sur
	sdlQuit

-- ゲームの状態
data GameState =
	GameState {
		pl :: Player
	}

-- 開始状態
initState :: GameState
initState =
	GameState {
		pl = newPlayer
		}


-- 毎フレームの処理
onProcess :: KeyProc -> GameState -> GameState
onProcess kp gs
	| otherwise		= gs { pl = updatePlayer (pl gs) kp}


{-
  wav <- loadWAV "snd/jump.wav"

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
-}


backColor = 0x2891ff


-- 描画処理
onDraw :: Surface -> ImageResource -> GameState -> IO ()
onDraw sur imgres gs = do
	fillRect sur Nothing backColor

	renderMap sur imgres
	renderPlayer sur (pl gs) imgres

	flipSurface sur
	return ()

