module AppUtil where

import Multimedia.SDL (Surface, SDLKey(..), loadBMP, freeSurface, surfacePixelFormat, displayFormat, pfPalette, setColorKey, SurfaceFlag(..), blitSurface, pt)
import Data.Maybe (fromJust)

import Const
import Images

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
getImageSurface imgres = fromJust . flip lookup imgres

putimg :: Surface -> ImageResource -> ImageType -> Int -> Int -> IO ()
putimg sur imgres imgtype x y = do
	blitSurface (getImageSurface imgres imgtype) Nothing sur (pt x y)
	return ()


-- 固定座標系からセル座標系に
cellCrd :: Int -> Int
cellCrd x = x `div` (chrSize * one)



-- ========
data Rect = Rect Int Int Int Int


ishit :: Rect -> Rect -> Bool
ishit (Rect l1 t1 r1 b1) (Rect l2 t2 r2 b2) =
	l1 < r2 && t1 < b2 && l2 < r1 && t2 < b1
