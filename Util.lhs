module Util where

import System.Time (getClockTime, diffClockTimes, noTimeDiff, tdMin, tdSec, tdPicosec)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Control.Concurrent (threadDelay)
import Multimedia.SDL

import Const

-- キーボード処理

data PadBtn =
	PadU | PadD | PadL | PadR | PadA | PadB
	deriving (Eq, Show, Enum)

data KeyState =
	Pushed | Pushing | Released | Releasing
	deriving (Eq, Show)

isPressed Pushed  = True
isPressed Pushing = True
isPressed _       = False

type KeyProc = PadBtn -> KeyState

keyProc bef cur gk
	| not bp && not cp = Releasing
	| not bp && cp     = Pushed
	| bp     && not cp = Released
	| bp     && cp     = Pushing
	where
		bp = any (flip elem bef) phykeys
		cp = any (flip elem cur) phykeys
		phykeys = mapPhyKey gk

mapPhyKey PadU = [SDLK_UP, SDLK_i]
mapPhyKey PadD = [SDLK_DOWN, SDLK_k]
mapPhyKey PadL = [SDLK_LEFT, SDLK_j]
mapPhyKey PadR = [SDLK_RIGHT, SDLK_l]
mapPhyKey PadA = [SDLK_SPACE]
mapPhyKey PadB = [SDLK_LSHIFT, SDLK_RSHIFT]

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



-- 画像リソース
type ImageResource = [(ImageType, Surface)]


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
