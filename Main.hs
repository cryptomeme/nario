-- Nario

module Main where

import Multimedia.SDL hiding (Event)
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Concurrent (threadDelay)

--import Control.Exception

import Player
import Field
import Util
import Const
import Font
import Event
import Actor

wndTitle = "NARIO in Haskell"
wndWidth = 256
wndHeight = 224
wndBpp = 32

frameRate = 60

-- 背景色
backColor = 0x5080FF

-- 描画コマンド
type Scr = Surface -> IO ()

-- エントリ
main :: IO ()
main = do
	sdlInit [VIDEO]
	setCaption wndTitle wndTitle
	sur <- setVideoMode wndWidth wndHeight wndBpp [HWSURFACE, DOUBLEBUF, ANYFORMAT]
	do
		strm <- delayedStream (1000000 `div` frameRate) fetch
		scrs <- process $ map snd $ takeWhile notQuit strm
		mapM_ (\scr -> scr sur) scrs
	sdlQuit

	where
		-- 環境のフェッチ
		fetch = do
			quit <- checkSDLEvent
			ks <- getKeyState
			return (quit, ks)
		notQuit = not . fst

-- 遅延ストリーム
-- microsec 秒ごとに func を実行したアクションの結果をリストとして返す
delayedStream :: Int -> IO a -> IO [a]
delayedStream microsec func = unsafeInterleaveIO $ do
	threadDelay microsec
	x <- func
	xs <- delayedStream microsec func
	return $ x:xs

-- SDL のイベントを処理
-- 終了イベントがきたら True を返す
checkSDLEvent = do
	ev <- pollEvent
	case ev of
		Just QuitEvent	-> return True
		Just (KeyboardEvent { kbPress = True, kbKeysym = Keysym { ksSym = ks, ksMod = km } })
			| ks == SDLK_ESCAPE -> return True
			| ks == SDLK_F4 && (KMOD_LALT `elem` km || KMOD_RALT `elem` km) -> return True
		Nothing	-> return False
		_		-> checkSDLEvent

----


-- 状態
data GameState = GameState {
	pl :: Player,
	fld :: Field,
	actors :: [Actor]
	}

-- 開始状態
initialState = do
	fldmap <- loadField stage
	return GameState {
		pl = newPlayer,
		fld = fldmap,
		actors = []
		}
	where
		stage = 0


-- キー入力を処理して描画コマンドを返す
process :: [[SDLKey]] -> IO [Scr]
process kss = do
	imgres <- loadImageResource images
	st <- initialState
	let scrs = map (\scr -> scr imgres) $ loop [] st kss
	return $ scrs ++ [(\sur -> do {releaseImageResource imgres})]
	where
		loop :: [SDLKey] -> GameState -> [[SDLKey]] -> [(ImageResource -> Scr)]
		loop _ _ [] = []
		loop bef gs (ks:kss) = scr' : loop ks gs' kss
			where
				(scr', gs') = updateProc kp gs
				kp = keyProc bef ks

-- 更新
updateProc :: KeyProc -> GameState -> (ImageResource -> Scr, GameState)
updateProc kp gs = (renderProc gs', gs')
	where
		(pl', ev) = updatePlayer kp (fld gs) (pl gs)
		actors_updates = map updateActor (actors gs)
		actors' = map fst actors_updates
		ev' = concatMap snd actors_updates

		gstmp = gs { pl = pl', actors = actors' }
		gs' = procEvent gstmp (ev ++ ev')

-- イベントを処理
procEvent :: GameState -> [Event] -> GameState
procEvent gs ev = foldl f gs ev
	where
		f gs (EvHitBlock imgtype cx cy) = gs { fld = fld', actors = actors' }
			where
				fld' = fieldSet (fld gs) cx cy '*'
				actors' = (newAnimBlock cx cy) : actors gs
		f gs (EvSetField cx cy c) = gs { fld = fld' }
			where
				fld' = fieldSet (fld gs) cx cy c


-- 描画
renderProc :: GameState -> ImageResource -> Scr
renderProc gs imgres sur = do
	fillRect sur Nothing backColor

	let scrx = getScrollPos (pl gs)

	renderField sur imgres scrx (fld gs)
	renderPlayer sur imgres scrx (pl gs)

	mapM_ (\act -> renderActor act imgres scrx sur) (actors gs)

	renderInfo gs imgres sur

	flipSurface sur
	return ()

-- 情報描画
renderInfo :: GameState -> ImageResource -> Scr
renderInfo gs imgres sur = do
	puts 3 1 "MARIO"
	puts 3 2 "000000"
	puts 11 2 "?*00"
	puts 18 1 "WORLD"
	puts 19 2 "1-1"
	puts 25 1 "TIME"
	puts 26 2 "255"
	where
		puts = fontPut sur fontsur
		fontsur = getImageSurface imgres ImgFont

-- タイトル画面
renderTitle gs imgres sur = do
	blitSurface (getImageSurface imgres ImgTitle) Nothing sur (pt (5*8) (3*8))
	puts 13 14 "@1985 NINTENDO"
	puts 9 17 "> 1 PLAYER GAME"
	puts 9 19 "  2 PLAYER GAME"
	puts 12 22 "TOP- 000000"
	where
		puts = fontPut sur fontsur
		fontsur = getImageSurface imgres ImgFont
