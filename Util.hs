module Util where

import System.Time (getClockTime, diffClockTimes, noTimeDiff, tdMin, tdSec, tdPicosec)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)
import Multimedia.SDL

import Const


-- ユーティリティ関数

-- 符号を返す
sgn x
	| x > 0		= 1
	| x < 0		= -1
	| otherwise	= 0

-- x に d を加算した結果が x0～x1 の範囲内を超えないようにする
-- もとから範囲外だったときはそれ以上遠ざからないように
rangeadd x d x0 x1
	| d > 0 && x < x1	= min (x + d) x1
	| d < 0 && x > x0	= max (x + d) x0
	| otherwise	= x


-- 値を０に近づける
friction x d
	| x > d		= x - d
	| x < -d	= x + d
	| otherwise	= 0



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
mapPhyKey PadA = [SDLK_SPACE, SDLK_z]
mapPhyKey PadB = [SDLK_LSHIFT, SDLK_RSHIFT]



-- 画像リソース
type ImageResource = [(ImageType, Surface)]


-- 画像リソース読み込み
loadImageResource :: [ImageType] -> IO ImageResource
loadImageResource = mapM load
	where
		load imgtype = do
			sur <- loadBMP $ ("data/img/" ++) $ imageFn imgtype
			setNuki sur
			converted <- displayFormat sur
			freeSurface sur
			return (imgtype, converted)

		setNuki sur = do
			let fmt = surfacePixelFormat sur
			if not $ null $ pfPalette fmt
				then setColorKey sur [SRCCOLORKEY] 0 >> return ()	-- パレット０番目をぬき色に
				else return ()

releaseImageResource :: ImageResource -> IO ()
releaseImageResource = mapM_ (\(t, sur) -> freeSurface sur)

getImageSurface :: ImageResource -> ImageType -> Surface
getImageSurface imgres t = fromJust $ lookup t imgres
