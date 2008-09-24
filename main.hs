
-- Nario

module Main where

import Data.List
import Data.IORef
import System.Time
import System.Random
import Control.Concurrent
import Control.Monad
import Multimedia.SDL

import Data.Maybe (fromJust)

-----------------------------------
-- システム周り 
-----------------------------------

-- 種種の定義

wndTitle = "Nario in Haskell"
wndSize  = sz 256 240


data ImageType = ImgNario00 | ImgNario01 | ImgNario02 | ImgNario03 | ImgNario04
	deriving Eq

imageFn ImgNario00 = "nario00.bmp"
imageFn ImgNario01 = "nario01.bmp"
imageFn ImgNario02 = "nario02.bmp"
imageFn ImgNario03 = "nario03.bmp"
imageFn ImgNario04 = "nario04.bmp"

images = [ImgNario00, ImgNario01, ImgNario02, ImgNario03, ImgNario04]



-- キーボード処理

data GameKey =
	GKUp | GKDown | GKLeft | GKRight | GKRotate
	deriving (Eq,Show,Enum)

data KeyState =
	Pushed | Pushing | Released | Releasing
	deriving (Eq,Show)

isPressed Pushed  = True
isPressed Pushing = True
isPressed _       = False

type KeyProc = GameKey -> KeyState

keyProc bef cur gk
	| not bp && not cp = Releasing
	| not bp && cp     = Pushed
	| bp     && not cp = Released
	| bp     && cp     = Pushing
	where
		bp = (mapPhyKey gk) `elem` bef
		cp = (mapPhyKey gk) `elem` cur

mapPhyKey GKUp     = SDLK_UP
mapPhyKey GKDown   = SDLK_DOWN
mapPhyKey GKLeft   = SDLK_LEFT
mapPhyKey GKRight  = SDLK_RIGHT
mapPhyKey GKRotate = SDLK_z

-- 時間調節

elapseTime :: Integer -> IO (IO (Int,Bool))
elapseTime fps = do
  let frametime = picosec `div` fps
  tm <- getClockTime
  st <- newIORef ((0,0,noTimeDiff), (1,tm))
  return $ do
    ((bef,cur,fdt), (cnt,bt)) <- readIORef st
    ct       <- getClockTime
    let dt   = diffClockTimes ct bt
        ndt  = diffClockTimes ct tm
        adj  = frametime*cnt - toPsec dt
        nc   = if cnt==fps then (1,ct) else (cnt+1,bt)
        (nbef,ncur) = if tdSec fdt /= tdSec ndt then (cur,0) else (bef,cur)
    if adj < 0 then do
        writeIORef st ((nbef,ncur,ndt), nc)
        return (bef, False)
      else do
        writeIORef st ((nbef,ncur+1,ndt), nc)
        threadDelay $ fromInteger $ min 16666 $ adj `div` 1000000
        return (bef, True)
  where
    toPsec dt = toInteger (tdMin dt * 60 + tdSec dt) * picosec + tdPicosec dt
    picosec = 1000000000000

-- メインループ

main :: IO ()
main = sdlStart [VIDEO] wndTitle wndSize $ \sur -> do
	is  <- initState
	gs  <- newIORef is
	res2 <- loadResource2

	et <- elapseTime 60
	loop et gs onProcess (onDraw sur res2) []

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
		dmy :: Int
	}

-- 開始状態
initState :: IO GameState
initState = do
	return GameState
		{ dmy = 0
		}



type Resource = (Surface,Surface,Surface,Surface, AudioData)

-- リソース読み込み
loadResource2 :: IO [(ImageType, Surface)]
loadResource2 = mapM f images
	where
		f t = do
			sur <- loadBMP $ ("img/" ++) $ imageFn t
			return (t, sur)

-- 毎フレームの処理
onProcess :: KeyProc -> GameState -> GameState
onProcess kp gs
	| otherwise		= gs { dmy = dmy gs + 1 }


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


-- 描画処理
onDraw :: Surface -> [(ImageType, Surface)] -> GameState -> IO ()
onDraw sur res gs = do
	fillRect sur Nothing 0x000000
	let c = lookup ImgNario00 res
	blitSurface (fromJust c) Nothing sur $ pt  70 20

	flipSurface sur
	return ()
