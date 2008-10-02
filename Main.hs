-- Nario

module Main where

import Multimedia.SDL hiding (Event)
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

wndTitle = "NARIO in Haskell"
screenWidth = 256
screenHeight = 224
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
	sur <- setVideoMode screenWidth screenHeight wndBpp [HWSURFACE, DOUBLEBUF, ANYFORMAT]
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
data GameGame =
	GameGame {
		pl :: Player,
		fld :: Field,
		actors :: [Actor],
		time :: Int
	}


-- キー入力を処理して描画コマンドを返す
process :: [[SDLKey]] -> IO [Scr]
process kss = do
	imgres <- loadImageResource imageTypes
	fldmap <- loadField 0

	let tmpscrs = doTitle fldmap kss

	let scrs = map (\scr sur -> scr imgres sur >> flipSurface sur >> return ()) $ tmpscrs
	return $ scrs ++ [(\sur -> do {releaseImageResource imgres})]


-- タイトル
doTitle :: Field -> [[SDLKey]] -> [ImageResource -> Scr]
doTitle fldmap kss = loop kss
	where
		loop :: [[SDLKey]] -> [ImageResource -> Scr]
		loop (ks:kss) = res : left ks kss

		res imgres sur = do
			fillRect sur Nothing backColor
			renderTitle imgres sur

		left ks kss
			| SDLK_SPACE `elem` ks	= doGame fldmap kss
			| otherwise				= loop kss

-- ゲーム
doGame :: Field -> [[SDLKey]] -> [ImageResource -> Scr]
doGame fldmap kss = loop (head kss) initialState kss
	where
		loop :: [SDLKey] -> GameGame -> [[SDLKey]] -> [ImageResource -> Scr]
		loop bef gs (ks:kss) = scr' : left ks kss
			where
				(scr', gs') = updateProc (keyProc bef ks) gs
				isPlayerDead = getPlayerYPos (pl gs') >= screenHeight + chrSize * 2
				timeOver = time gs' <= 0

				left ks kss
					| isPlayerDead || timeOver	= doGameOver fldmap kss
					| otherwise					= loop ks gs' kss

		-- 更新
		updateProc :: KeyProc -> GameGame -> (ImageResource -> Scr, GameGame)
		updateProc kp gs = (renderProc gs', gs')
			where
				time' = max 0 (time gs - one `div` 25)
				(pl', ev) = updatePlayer kp (fld gs) (pl gs)
				actors_updates = map updateActor (actors gs)
				actors' = map fst actors_updates
				ev' = concatMap snd actors_updates

				gstmp = gs { pl = pl', actors = actors', time = time' }
				gs' = procEvent gstmp (ev ++ ev')

		initialState = GameGame { pl = newPlayer, fld = fldmap, actors = [], time = 400 * one }

doGameOver fldmap kss = doTitle fldmap kss


-- イベントを処理
procEvent :: GameGame -> [Event] -> GameGame
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
renderProc :: GameGame -> ImageResource -> Scr
renderProc gs imgres sur = do
	fillRect sur Nothing backColor

	let scrx = getScrollPos (pl gs)

	renderField sur imgres scrx (fld gs)
	renderInfo gs imgres sur
	renderPlayer sur imgres scrx (pl gs)

	mapM_ (\act -> renderActor act imgres scrx sur) (actors gs)
	return ()

tailN n = reverse . take n . reverse

deciWide w c n = tailN w $ replicate w c ++ show n

-- 情報描画
renderInfo :: GameGame -> ImageResource -> Scr
renderInfo gs imgres sur = do
	puts 3 1 "MARIO"
	puts 3 2 $ deciWide 6 '0' $ getPlayerScore (pl gs)
	puts 11 2 ("?*" ++ deciWide 2 '0' (getPlayerMedal (pl gs)))
	puts 18 1 "WORLD"
	puts 19 2 "1-1"
	puts 25 1 "TIME"
	puts 26 2 $ deciWide 3 ' ' ((time gs + one-1) `div` one)
	where
		puts = fontPut sur fontsur
		fontsur = getImageSurface imgres ImgFont

-- タイトル画面
renderTitle imgres sur = do
	blitSurface (getImageSurface imgres ImgTitle) Nothing sur (pt (5*8) (3*8))
	puts 13 14 "@1985 NINTENDO"
	puts 9 17 "> 1 PLAYER GAME"
--	puts 9 19 "  2 PLAYER GAME"
	puts 12 22 "TOP- 000000"
	where
		puts = fontPut sur fontsur
		fontsur = getImageSurface imgres ImgFont
