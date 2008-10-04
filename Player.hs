-- プレーヤー

module Player (
	Player(..),
	PlayerType(..),
	newPlayer,
	updatePlayer,
	renderPlayer,
	getScrollPos,
	getPlayerYPos,
	getPlayerVY,
	getPlayerHitRect,
	getPlayerMedal,
	getPlayerScore,
	getPlayerType,
	setPlayerType,
	setPlayerDamage,
	stampPlayer
) where

import Multimedia.SDL (blitSurface, pt)
import Data.Bits ((.&.))

import Util
import AppUtil (KeyProc, isPressed, PadBtn(..), cellCrd, KeyState(..), getImageSurface, Rect(..))
import Const
import Images
import Field
import Event


walkVx = one * 3 `div` 2
runVx = one * 3
maxVy = one * 6
acc = one `div` 6
jumpVy = -13 * gravity
scrollMinX = 5 * chrSize
scrollMaxX = 8 * chrSize
gravity2 = one `div` 4		-- Aを長押ししたときの重力
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

	medal :: Int,
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

	medal = 0,
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
	if (stand self)
		then self' { lr = lr', pat = pat', anm = anm' }
		else self'
	where
		ax = if padd then 0 else (-padl + padr) * acc
		vx'
			| ax /= 0			= rangeadd (vx self) ax (-maxspd) maxspd
			| stand self		= friction (vx self) acc
			| otherwise			= vx self
		x' = max xmin $ (x self) + vx'
		scrx'
			| vx' > 0 && (x' - (scrx self)) `div` one > scrollPos	= (scrx self) + vx'
			| otherwise												= (scrx self)

		scrollPos = (max vx' 0) * (scrollMaxX - scrollMinX) `div` runVx + scrollMinX

		padd = if isPressed (kp PadD) then True else False
		padl = if isPressed (kp PadL) then 1 else 0
		padr = if isPressed (kp PadR) then 1 else 0
		maxspd
			| not $ stand self	= walkVx `div` 2
			| isPressed (kp PadB)	= walkVx * 2
			| otherwise				= walkVx
		xmin = (scrx self) + chrSize `div` 2 * one

		self' = self { x = x', vx = vx', scrx = scrx' }

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
	| otherwise = (self { vy = 0, score = (score self) + 10 }, [EvHitBlock ImgBlock2 cx cy (pltype self /= SmallNario)])
	where
		yofs = case pltype self of
			SmallNario	-> 15
			SuperNario	-> 30
			FireNario	-> 30
		ytmp = y self - yofs * one

		cx = cellCrd $ x self
		cy = cellCrd ytmp
		isCeil = isBlock $ fieldRef fld cx cy
		yground y = (cellCrd y) * (chrSize * one)


-- ジャンプする？
doJump :: KeyProc -> Player -> Player
doJump kp self
	| stand self && kp PadA == Pushed	= self { vy = jumpVy, stand = False, pat = patJump }
	| otherwise							= self


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
	moveY $ checkX fld $ moveX kp self
	where
		moveY = checkCeil fld . doJump kp . checkFloor fld . fall (isPressed $ kp PadA)

-- 死亡時
updateDead :: KeyProc -> Field -> Player -> (Player, [Event])
updateDead kp fld self = (fall False self, [])

-- スクロール位置取得
getScrollPos :: Player -> Int
getScrollPos self = (scrx self) `div` one

-- Ｙ座標取得
getPlayerYPos :: Player -> Int
getPlayerYPos = (`div` one) . y

-- Ｙ速度取得
getPlayerVY :: Player -> Int
getPlayerVY = vy

-- 当たり判定用矩形
getPlayerHitRect :: Player -> Rect
getPlayerHitRect self = Rect (xx - 6) (yy - 16) (xx + 6) yy
	where
		xx = x self `div` one
		yy = y self `div` one

-- メダル枚数取得
getPlayerMedal :: Player -> Int
getPlayerMedal = medal

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

-- 描画
renderPlayer sur imgres scrx self = do
	if undeadCount self == 0 || (undeadCount self .&. 1) /= 0
		then blitSurface (getImageSurface imgres imgtype) Nothing sur pos >> return ()
		else return ()
	where
		pos = case pltype self of
			SmallNario	-> pt sx $ sy - chrSize + 1
			otherwise	-> pt sx $ sy - chrSize * 2 + 1
		imgtype
			| plstate self == Dead	= ImgNarioDead
			| otherwise				= imgtbl !! lr self !! pat self
		imgtbl = case pltype self of
			SmallNario	-> imgTableSmall
			SuperNario	-> imgTableSuper
			FireNario	-> imgTableFire
		sx = x self `div` one - chrSize `div` 2 - scrx
		sy = y self `div` one - 8
