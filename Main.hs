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
import Actor.AnimBlock
import Actor.Kuribo
import Actor.Nokonoko
import Actor.Kinoko
import Actor.Flower
import Actor.BrokenBlock
import Actor.CoinGet
import Actor.ScoreAdd
import Mixer

-- 背景色
backColor = 0x5080FF

-- 描画コマンド
type Scr = Surface -> Mixer -> IO ()


-- エントリ
main :: IO ()
main = do
	sdlInit [VIDEO]
	setCaption wndTitle wndTitle
	sur <- setVideoMode screenWidth screenHeight wndBpp [HWSURFACE, DOUBLEBUF, ANYFORMAT]
	do
		mixer <- createMixer
		strm <- delayedStream (1000000 `div` frameRate) fetch
		scrs <- process $ map snd $ takeWhile notQuit strm
		mapM_ (\scr -> scr sur mixer) scrs
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


-- 状態
data GameGame =
	GameGame {
		pl :: Player,
		fld :: Field,
		actors :: [ActorWrapper],
		time :: Int,
		snds :: [SoundType]
	}


-- キー入力全体を処理して描画コマンド列を返す
process :: [[SDLKey]] -> IO [Scr]
process kss = do
	imgres <- loadImageResource imageTypes
	sndres <- loadSoundResource soundTypes
	fldmap <- loadField 0

	let tmpscrs = doTitle fldmap kss
	let scrs = zipWith (common imgres sndres) tmpscrs kss
	return $ scrs ++ [final imgres sndres]

	where
		-- 共通動作
		common imgres sndres scr ks sur mixer = do
			scr imgres sndres sur mixer
			if SDLK_s `elem` ks
				then saveBMP sur "ss.bmp" >> return ()
				else return ()
			flipSurface sur
			return ()
		-- 後始末
		final imgres sndres sur mixer = releaseImageResource imgres

-- タイトル
doTitle :: Field -> [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
doTitle fldmap kss = loop kss
	where
		loop :: [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
		loop (ks:kss) = res : left ks kss

		res imgres sndres sur mixer = do
			fillRect sur Nothing backColor
			renderTitle imgres sur

		left ks kss
			| SDLK_SPACE `elem` ks	= doGame fldmap kss
			| otherwise				= loop kss


-- マップのスクロールに応じたイベント
scrollEvent :: Field -> Int -> (Field, [Event])
scrollEvent fld cx
	| cx < length (head fld)	= foldl proc (fld, []) $ zip [0..] cols
	| otherwise					= (fld, [])
	where
		proc (f, e) (cy, c) =
			case event cy c of
				Just ev	-> (fieldSet f cx cy ' ', ev : e)
				Nothing	-> (f, e)
		cols = map (!! cx) fld
		event cy c
			| c `elem` "kn"	= Just $ EvAddActor $ genActor c
			| otherwise		= Nothing
			where
				genActor c = case c of
					'k'	-> ActorWrapper $ newKuribo cx cy
					'n'	-> ActorWrapper $ newNokonoko cx cy



-- 当たり判定
hitcheck :: Player -> [ActorWrapper] -> (Player, [ActorWrapper], [Event])
hitcheck player actors = foldl proc (player, [], []) actors
	where
		proc (pl, ac, ev) (ActorWrapper a) = case getHitRect a of
			Nothing	-> nothingHappened
			Just rc	->
				if not $ ishit plrc rc
					then nothingHappened
					else (pl', ac', ev')
			where
				nothingHappened = (pl, ac ++ [ActorWrapper a], ev)
				plrc = getPlayerHitRect player
				(pl', a', evtmp) = onHit pl a
				ac' = case a' of
					Just a''	-> ac ++ [a'']
					Nothing		-> ac
				ev' = ev ++ evtmp


-- ゲーム
doGame :: Field -> [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
doGame fldmap kss = loop (head kss) initialState kss
	where
		loop :: [SDLKey] -> GameGame -> [[SDLKey]] -> [ImageResource -> SoundResource -> Scr]
		loop bef gs (ks:kss) = scr' : left ks kss
			where
				(scr', gs') = updateProc (keyProc bef ks) gs
				isPlayerDead = getPlayerY (pl gs') >= (screenHeight + chrSize * 2) * one
				timeOver = time gs' <= 0

				left ks kss
					| isPlayerDead || timeOver	= doGameOver fldmap kss
					| otherwise					= loop ks gs' kss

		-- 更新
		updateProc :: KeyProc -> GameGame -> (ImageResource -> SoundResource -> Scr, GameGame)
		updateProc kp gs = (scr', gs')
			where
				time' = max 0 (time gs - 1)
				(fld', screv') = scrollEvent (fld gs) $ getScrollPos (pl gs) `div` chrSize + 18

				(pl', plev) = updatePlayer kp fld' (pl gs)
				actors_updates = updateActors (fld gs) (actors gs)
				actors' = filterActors $ map fst actors_updates
				ev' = concatMap snd actors_updates

				(pl'', actors'', ev'') = hitcheck pl' actors'

				gstmp = gs { pl = pl'', fld = fld', actors = actors'', time = time' }
				allEvent = plev ++ ev' ++ screv' ++ ev''
				gs' = procEvent gstmp allEvent
				scr' imgres sndres sur mixer = do
					mapM_ (\ev -> case ev of
							EvSound sndtype	->	play sndtype
							otherwise		->	return ()
						) allEvent
					renderProc gs' imgres sndres sur mixer

					where
						play sndtype = do
							if False
								then
									playWav mixer $ lookup sndtype sndres
								else do
									-- 実際の playWav を呼び出すとたまに落ちるので、print に変更
									putStrLn $ "play " ++ show sndtype
									return ()

		initialState = GameGame { pl = newPlayer, fld = fldmap, actors = [], time = 400 * timeBase, snds = [] }


-- ゲームオーバー
doGameOver fldmap kss = doTitle fldmap kss


-- イベントを処理
procEvent :: GameGame -> [Event] -> GameGame
procEvent gs ev = foldl proc gs ev
	where
		proc gs (EvHitBlock imgtype cx cy bSuper)
			| hardBlock c			= gs
			| bSuper && breakable c	= breakBlock gs cx cy
			| c == 'K'				= genKinoko
			| c == '?'				= getCoin
			| otherwise				= gs'
			where
				c = fieldRef (fld gs) cx cy
				breakable c = c == 'O'

				gs' = gs { fld = fld', actors = actors' }
				actors' = actors gs ++ [ActorWrapper $ newAnimBlock cx cy $ fieldRef (fld gs) cx cy]
				fld' = fieldSet (fld gs) cx cy '*'

				breakBlock gs cx cy =
					gs {
						fld = fieldSet (fld gs) cx cy ' ',
						actors = actors gs ++ map ActorWrapper (newBrokenBlock cx cy),
						pl = addScore pointBreakBlock $ pl gs
						}
				genKinoko = gs' { actors = actors gs' ++ [a] }
					where a = if not bSuper then ActorWrapper $ newKinoko cx cy else ActorWrapper $ newFlower cx cy
				getCoin = gs' { actors = actors gs' ++ [ActorWrapper a], pl = addScore pointGetCoin $ playerGetCoin $ pl gs' }
					where a = newCoinGet cx cy

		proc gs (EvSetField cx cy c) = gs { fld = fieldSet (fld gs) cx cy c }
		proc gs (EvAddActor act) = gs { actors = actors gs ++ [act] }
		proc gs (EvScoreAddEfe sx sy pnt) = gs { actors = actors gs ++ [ActorWrapper $ newScoreAdd sx sy pnt] }
		proc gs (EvSound sndtype) = gs

-- 描画
renderProc :: GameGame -> ImageResource -> SoundResource -> Scr
renderProc gs imgres sndres sur mixer = do
	fillRect sur Nothing backColor

	let scrx = getScrollPos (pl gs)

	renderField sur imgres scrx (fld gs)
	renderInfo gs imgres sur
	renderActors imgres scrx sur (actors gs)
	renderPlayer sur imgres scrx (pl gs)

	return ()

-- 情報描画
renderInfo gs imgres sur = do
	puts  3 1 "NARIO"
	puts  3 2 $ deciWide 6 '0' $ getPlayerScore (pl gs)
	puts 11 2 ("?*" ++ deciWide 2 '0' (getPlayerCoin $ pl gs))
	puts 18 1 "WORLD"
	puts 19 2 "1-1"
	puts 25 1 "TIME"
	puts 26 2 $ deciWide 3 '0' $ (time gs + timeBase-1) `div` timeBase
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16

-- タイトル画面
renderTitle imgres sur = do
	putimg sur imgres ImgTitle (5*8) (3*8)
--	puts 13 14 "@1985 NINTENDO"
	puts  9 17 "> 1 PLAYER GAME"
--	puts  9 19 "  2 PLAYER GAME"
	puts 12 22 "TOP- 000000"
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16
