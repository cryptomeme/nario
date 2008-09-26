module Util where

import System.Time (getClockTime, diffClockTimes, noTimeDiff, tdMin, tdSec, tdPicosec)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Control.Concurrent (threadDelay)
import Multimedia.SDL

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
