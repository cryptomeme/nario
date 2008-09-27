
-- Nario

module Main where

import Multimedia.SDL
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)

import SDLUtil
import Util
import Player
import Field
import Const

-----------------------------------
-- システム周り 
-----------------------------------

-- 種種の定義

wndTitle = "Nario in Haskell"
wndSize  = sz 256 240

-- 背景色
backColor = 0x2891ff		-- 青




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
		Just QuitEvent	-> return True
		Just (KeyboardEvent { kbPress = True, kbKeysym = Keysym { ksSym = ks, ksMod = km } })
			| ks == SDLK_ESCAPE -> return True
			| ks == SDLK_F4 && (KMOD_LALT `elem` km ||
								KMOD_RALT `elem` km) -> return True
		Nothing			-> return False
		_				-> checkEvent


sdlStart fs title (Size w h) p = do
	True <- sdlInit fs
	setCaption title title
	sur  <- setVideoMode w h 32 [HWSURFACE,DOUBLEBUF,ANYFORMAT]
	p sur
	sdlQuit

-- ゲームの状態
data GameState =
	GameState {
		pl :: Player,
		fld :: Field
	}

-- 開始状態
initState :: GameState
initState =
	GameState {
		pl = newPlayer,
		fld = getField stage
		}
	where
		stage = 0


-- 毎フレームの処理
onProcess :: KeyProc -> GameState -> GameState
onProcess kp gs
	| otherwise		= gs { pl = updatePlayer kp (fld gs) (pl gs) }


-- 描画処理
onDraw :: Surface -> ImageResource -> GameState -> IO ()
onDraw sur imgres gs = do
	fillRect sur Nothing backColor

	renderField sur imgres
	renderPlayer sur (pl gs) imgres

	flipSurface sur
	return ()

