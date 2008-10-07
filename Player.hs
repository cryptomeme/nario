-- プレーヤー

module Player (
	Player(..),
	PlayerType(..),
	newPlayer,
	updatePlayer,
	renderPlayer,
	playerGetCoin,
	addScore,
	getScrollPos,
	getPlayerX,
	getPlayerY,
	getPlayerVY,
	getPlayerHitRect,
	getPlayerCoin,
	getPlayerScore,
	getPlayerType,
	setPlayerType,
	setPlayerDamage,
	stampPlayer
) where

import Multimedia.SDL (blitSurface, pt)
import Data.Bits ((.&.))

import Util
import AppUtil (KeyProc, padPressed, padPressing, PadBtn(..), cellCrd, KeyState(..), getImageSurface, Rect(..), putimg)
import Const
import Images
import Field
import Event


walkVx = one * 4 `div` 2
runVx = one * 11 `div` 4
maxVy = one * 5
acc = one `div` 32
acc2 = one `div` 14
jumpVy = -12 * gravity
jumpVy2 = -13 * gravity
scrollMinX = 5 * chrSize + 6
scrollMaxX = 8 * chrSize
gravity2 = one `div` 6		-- Aを長押ししたときの重力
stampVy = -8 * gravity
undeadFrame = frameRate * 2

-- 種類
data PlayerType = SmallNario | SuperNario | FireNario
	deriving (Eq)

-- 状態
data PlayerState = Normal | Dead
	deriving (Eq)

-- 構造体
data Player = Player {
	pltype :: PlayerType,
	plstate :: PlayerState,
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	scrx :: Int,
	stand :: Bool,
	undeadCount :: Int,

	coin :: Int,
	score :: Int,

	lr :: Int,
	pat :: Int,
	anm :: Int
	}

newPlayer = Player {
	pltype = SmallNario,
	plstate = Normal,
	x = 3 * chrSize * one,
	y = 13 * chrSize * one,
	vx = 0,
	vy = 0,
	scrx = 0,
	stand = False,
	undeadCount = 0,

	coin = 0,
	score = 0,

	lr = 1,
	pat = 0,
	anm = 0
	}


patStop = 0
patWalk = 1
walkPatNum = 3
patJump = patWalk + walkPatNum
patSlip = patJump + 1
patSit = patSlip + 1
patShot = patSit + 1
patDead = patShot + 2

imgTableSmall = [
	[ImgNarioLStand, ImgNarioLWalk1, ImgNarioLWalk2, ImgNarioLWalk3, ImgNarioLJump, ImgNarioLSlip, ImgNarioLStand],
	[ImgNarioRStand, ImgNarioRWalk1, ImgNarioRWalk2, ImgNarioRWalk3, ImgNarioRJump, ImgNarioRSlip, ImgNarioRStand]
	]
imgTableSuper = [
	[ImgSNarioLStand, ImgSNarioLWalk1, ImgSNarioLWalk2, ImgSNarioLWalk3, ImgSNarioLJump, ImgSNarioLSlip, ImgSNarioLSit],
	[ImgSNarioRStand, ImgSNarioRWalk1, ImgSNarioRWalk2, ImgSNarioRWalk3, ImgSNarioRJump, ImgSNarioRSlip, ImgSNarioRSit]
	]
imgTableFire = [
	[ImgFNarioLStand, ImgFNarioLWalk1, ImgFNarioLWalk2, ImgFNarioLWalk3, ImgFNarioLJump, ImgFNarioLSlip, ImgFNarioLSit, ImgFNarioLShot],
	[ImgFNarioRStand, ImgFNarioRWalk1, ImgFNarioRWalk2, ImgFNarioRWalk3, ImgFNarioRJump, ImgFNarioRSlip, ImgFNarioRSit, ImgFNarioRShot]
	]


-- 横移動
moveX :: KeyProc -> Player -> Player
moveX kp self =
	if stand self
		then self' { lr = lr', pat = pat', anm = anm' }
		else self'
	where
		axtmp = if padd then 0 else (-padl + padr) * nowacc
		ax = if sgn (axtmp * vx self) < 0 then axtmp * 2 else axtmp
		vx'
			| ax /= 0			= rangeadd (vx self) ax (-maxspd) maxspd
			| stand self		= friction (vx self) acc
			| otherwise			= vx self
		x' = max xmin $ (x self) + vx'

		padd = if padPressing kp PadD then True else False
		padl = if padPressing kp PadL then 1 else 0
		padr = if padPressing kp PadR then 1 else 0
		maxspd
			| not $ stand self	= walkVx `div` 2
			| padPressing kp PadB	= runVx
			| otherwise				= walkVx
		nowacc
			| padPressing kp PadB	= acc2
			| otherwise				= acc
		xmin = (scrx self + chrSize `div` 2) * one

		self' = self { x = x', vx = vx' }

		lr' =
			case (-padl + padr) of
				0	-> lr self
				-1	-> 0
				1	-> 1
		pat'
			| padd && pltype self /= SmallNario	= patSit
			| vx' == 0				= patStop
			| vx' > 0 && lr' == 0	= patSlip
			| vx' < 0 && lr' == 1	= patSlip
			| otherwise				= (anm' `div` anmCnt) + patWalk
		anm'
			| vx' == 0		= 0
			| otherwise		= ((anm self) + (abs vx')) `mod` (walkPatNum * anmCnt)
		anmCnt = walkVx * 3

-- 横移動チェック
checkX :: Field -> Player -> Player
checkX fld self
	| dir == 0	= check (-1) $ check 1 $ self
	| otherwise = check dir $ self
	where
		dir = sgn $ vx self
		check dx self
			| isBlock $ fieldRef fld cx cy	= self { x = (x self) - dx * one, vx = 0 }
			| otherwise						= self
			where
				cx = cellCrd (x self + ofsx dx)
				cy = cellCrd (y self - chrSize `div` 2 * one)
				ofsx (-1) = -6 * one
				ofsx   1  =  5 * one

-- スクロール
scroll :: Player -> Player -> Player
scroll opl self = self { scrx = scrx' }
	where
		odx = x opl `div` one - scrx opl
		dx = (max 0 $ vx self) * (scrollMaxX - scrollMinX) `div` runVx + scrollMinX
		scrx'
			| d > 0		= scrx self + d
			| otherwise	= scrx self
		d = x self `div` one - scrx self - (max odx dx)


-- 重力による落下
fall :: Bool -> Player -> Player
fall abtn self
	| stand self	= self
	| otherwise		= self { y = y', vy = vy' }
	where
		ay
			| vy self < 0 && abtn	= gravity2
			| otherwise				= gravity
		vy' = min maxVy $ vy self + ay
		y' = y self + vy'


-- 床をチェック
checkFloor :: Field -> Player -> Player
checkFloor fld self
	| stand'	= self { stand = stand', y = ystand, vy = 0 }
	| otherwise	= self { stand = stand' }
	where
		stand'
			| vy self >= 0	= isGround (-6) || isGround 5
			| otherwise			= stand self
		ystand = (cellCrd $ y self) * (chrSize * one)

		isGround ofsx = isBlock $ fieldRef fld (cellCrd $ x self + ofsx * one) (cellCrd (y self))

-- 上をチェック
checkCeil :: Field -> Player -> (Player, [Event])
checkCeil fld self
	| stand self || vy self >= 0 || not isCeil	= (self, [])
	| otherwise = (self { y = y', vy = 0 }, [EvHitBlock ImgBlock2 cx cy (pltype self /= SmallNario)])
	where
		yofs = case pltype self of
			SmallNario	-> 14
			SuperNario	-> 28
			FireNario	-> 28
		ytmp = y self - yofs * one

		cx = cellCrd $ x self
		cy = cellCrd ytmp
		isCeil = isBlock $ fieldRef fld cx cy
		yground y = (cellCrd y) * (chrSize * one)
		y' = ((cy + 1) * chrSize + yofs) * one

-- ジャンプする？
doJump :: KeyProc -> Player -> Player
doJump kp self
	| stand self && padPressed kp PadA	= self { vy = vy', stand = False, pat = patJump }
	| otherwise							= self
	where
		vy' = (jumpVy2 - jumpVy) * (abs $ vx self) `div` runVx + jumpVy

-- 更新処理
updatePlayer :: KeyProc -> Field -> Player -> (Player, [Event])
updatePlayer kp fld self =
	case plstate self of
		Normal	-> updateNormal kp fld self'
		Dead	-> updateDead kp fld self'
	where
		self' = decUndead self
		decUndead pl = pl { undeadCount = max 0 $ undeadCount pl - 1 }

-- 通常時
updateNormal :: KeyProc -> Field -> Player -> (Player, [Event])
updateNormal kp fld self =
	moveY $ scroll self $ checkX fld $ moveX kp self
	where
		moveY = checkCeil fld . doJump kp . checkFloor fld . fall (padPressing kp PadA)

-- 死亡時
updateDead :: KeyProc -> Field -> Player -> (Player, [Event])
updateDead kp fld self = (fall False self, [])

-- スクロール位置取得
getScrollPos :: Player -> Int
getScrollPos = scrx

-- Ｘ座標取得
getPlayerX :: Player -> Int
getPlayerX = x

-- Ｙ座標取得
getPlayerY :: Player -> Int
getPlayerY = y

-- Ｙ速度取得
getPlayerVY :: Player -> Int
getPlayerVY = vy

-- 当たり判定用矩形
getPlayerHitRect :: Player -> Rect
getPlayerHitRect self = Rect (xx - 6) (yy - 16) (xx + 6) yy
	where
		xx = x self `div` one
		yy = y self `div` one

-- コイン枚数取得
getPlayerCoin :: Player -> Int
getPlayerCoin = coin

-- スコア取得
getPlayerScore :: Player -> Int
getPlayerScore = score

-- タイプ取得
getPlayerType :: Player -> PlayerType
getPlayerType = pltype

-- タイプ設定
setPlayerType :: PlayerType -> Player -> Player
setPlayerType t self = self { pltype = t }

-- ダメージを与える
setPlayerDamage :: Player -> Player
setPlayerDamage self
	| undeadCount self > 0			= self
	| pltype self == SmallNario		= self { plstate = Dead, pat = patDead, vy = jumpVy, stand = False }
	| otherwise						= self { pltype = SmallNario, undeadCount = undeadFrame }

-- 敵を踏み潰した
stampPlayer :: Player -> Player
stampPlayer self = self { vy = stampVy }

-- コイン取得
playerGetCoin :: Player -> Player
playerGetCoin self = self { coin = (coin self + 1) `mod` 100 }

-- スコア加算
addScore :: Int -> Player -> Player
addScore a self = self { score = score self + a }

-- 描画
renderPlayer sur imgres scrx self = do
	if undeadCount self == 0 || (undeadCount self .&. 1) /= 0
		then putimg sur imgres imgtype sx posy
		else return ()
	where
		posy = case pltype self of
			SmallNario	-> sy - chrSize + 1
			otherwise	-> sy - chrSize * 2 + 1
		imgtype
			| plstate self == Dead	= ImgNarioDead
			| otherwise				= imgtbl !! lr self !! pat self
		imgtbl = case pltype self of
			SmallNario	-> imgTableSmall
			SuperNario	-> imgTableSuper
			FireNario	-> imgTableFire
		sx = x self `div` one - chrSize `div` 2 - scrx
		sy = y self `div` one - 8
