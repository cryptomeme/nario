-- プレーヤー

module Player (
	Player(..),
	newPlayer,
	updatePlayer,
	renderPlayer,
	getScrollPos,
	getPlayerYPos,
	getPlayerMedal,
	getPlayerScore
) where

import Multimedia.SDL hiding (Event)

import Util
import AppUtil
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

data Player = Player {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	scrx :: Int,
	stand :: Bool,

	medal :: Int,
	score :: Int,

	lr :: Int,
	pat :: Int,
	anm :: Int
	}

newPlayer = Player {
	x = 3 * chrSize * one,
	y = 13 * chrSize * one,
	vx = 0,
	vy = 0,
	scrx = 0,
	stand = False,

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

imgTable = [
	[ImgNarioLStand, ImgNarioLWalk1, ImgNarioLWalk2, ImgNarioLWalk3, ImgNarioLJump, ImgNarioLSlip],
	[ImgNarioRStand, ImgNarioRWalk1, ImgNarioRWalk2, ImgNarioRWalk3, ImgNarioRJump, ImgNarioRSlip]
	]


-- 横移動
moveX :: KeyProc -> Player -> Player
moveX kp player =
	if (stand player)
		then player' { lr = lr', pat = pat', anm = anm' }
		else player'
	where
		ax = (-padl + padr) * acc
		vx'
			| ax /= 0			= rangeadd (vx player) ax (-maxspd) maxspd
			| stand player		= friction (vx player) acc
			| otherwise			= vx player
		x' = max xmin $ (x player) + vx'
		scrx'
			| vx' > 0 && (x' - (scrx player)) `div` one > scrollPos	= (scrx player) + vx'
			| otherwise												= (scrx player)

		scrollPos = (max vx' 0) * (scrollMaxX - scrollMinX) `div` runVx + scrollMinX

		padl = if isPressed (kp PadL) then 1 else 0
		padr = if isPressed (kp PadR) then 1 else 0
		maxspd
			| not $ stand player	= walkVx `div` 2
			| isPressed (kp PadB)	= walkVx * 2
			| otherwise				= walkVx
		xmin = (scrx player) + chrSize `div` 2 * one

		player' = player { x = x', vx = vx', scrx = scrx' }

		lr' =
			case (-padl + padr) of
				0	-> lr player
				-1	-> 0
				1	-> 1
		pat'
			| vx' == 0				= patStop
			| vx' > 0 && lr' == 0	= patSlip
			| vx' < 0 && lr' == 1	= patSlip
			| otherwise				= (anm' `div` anmCnt) + patWalk
		anm'
			| vx' == 0		= 0
			| otherwise		= ((anm player) + (abs vx')) `mod` (walkPatNum * anmCnt)
		anmCnt = walkVx * 3


-- 横移動チェック
checkX :: Field -> Player -> Player
checkX fld player
	| dir == 0	= check (-1) $ check 1 $ player
	| otherwise = check dir $ player
	where
		dir = sgn $ vx player
		check dx player
			| isBlock $ fieldRef fld cx cy	= player { x = (x player) - dx * one, vx = 0 }
			| otherwise						= player
			where
				cx = cellCrd (x player + ofsx dx)
				cy = cellCrd (y player - chrSize `div` 2 * one)
				ofsx (-1) = -6 * one
				ofsx   1  =  5 * one


-- 重力による落下
fall :: KeyProc -> Player -> Player
fall kp player
	| stand player	= player
	| otherwise		= player { y = y', vy = vy' }
	where
		ay
			| vy player < 0 && isPressed (kp PadA)	= gravity2
			| otherwise								= gravity
		vy' = min maxVy $ vy player + ay
		y' = y player + vy'


-- 床をチェック
checkFloor :: Field -> Player -> Player
checkFloor fld player
	| stand'	= player { stand = stand', y = ystand, vy = 0 }
	| otherwise	= player { stand = stand' }
	where
		stand'
			| vy player >= 0	= isGround (-6) || isGround 5
			| otherwise			= stand player
		ystand = (cellCrd $ y player) * (chrSize * one)

		isGround ofsx = isBlock $ fieldRef fld (cellCrd $ x player + ofsx * one) (cellCrd (y player))

-- 上をチェック
checkCeil :: Field -> Player -> (Player, [Event])
checkCeil fld player
	| stand player || vy player >= 0 || not isCeil	= (player, [])
	| otherwise = (player { vy = 0, score = (score player) + 10 }, [EvHitBlock ImgBlock2 cx cy])
	where
		ytmp = y player - one * chrSize

		cx = cellCrd $ x player
		cy = cellCrd ytmp
		isCeil = isBlock $ fieldRef fld cx cy
		yground y = (cellCrd y) * (chrSize * one)


-- ジャンプする？
doJump :: KeyProc -> Player -> Player
doJump kp player
	| stand player && kp PadA == Pushed	= player { vy = jumpVy, stand = False, pat = patJump }
	| otherwise							= player


-- 更新処理
updatePlayer :: KeyProc -> Field -> Player -> (Player, [Event])
updatePlayer kp fld player =
	moveY $ checkX fld $ moveX kp player
	where
		moveY = checkCeil fld . doJump kp . checkFloor fld . fall kp

-- スクロール位置取得
getScrollPos :: Player -> Int
getScrollPos player = (scrx player) `div` one

-- Ｙ座標取得
getPlayerYPos :: Player -> Int
getPlayerYPos = (`div` one) . y

-- メダル枚数取得
getPlayerMedal :: Player -> Int
getPlayerMedal = medal

-- スコア取得
getPlayerScore :: Player -> Int
getPlayerScore = score

-- 描画
renderPlayer sur imgres scrx player = do
	blitSurface (getImageSurface imgres imgtype) Nothing sur pos
	where
		pos = pt ((x player) `div` one - chrSize `div` 2 - scrx) ((y player) `div` one - chrSize+1 - 8)
		imgtype = imgTable !! (lr player) !! (pat player)
