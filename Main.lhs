
-- Nario

module Main where

import Multimedia.SDL
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromJust)

import SDLUtil
import Util
import Player

-----------------------------------
-- システム周り 
-----------------------------------

-- 種種の定義

wndTitle = "Nario in Haskell"
wndSize  = sz 256 240

-- 背景色
backColor = 0x2891ff		-- 青






-- マップ

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


-- 描画処理
onDraw :: Surface -> ImageResource -> GameState -> IO ()
onDraw sur imgres gs = do
	fillRect sur Nothing backColor

	renderMap sur imgres
	renderPlayer sur (pl gs) imgres

	flipSurface sur
	return ()

