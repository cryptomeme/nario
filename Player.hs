
-- プレーヤー

module Player (
	Player(..),
	newPlayer,
	updatePlayer,
	renderPlayer,
	getScrollPos
) where

import Multimedia.SDL

import Util
import SDLUtil
import Const
import Field


maxVx = one * 3
maxVy = one * 8
acc = one `div` 6
jumpVy = -17 * gravity


data Player = Player {
	x :: Int,
	y :: Int,
	vx :: Int,
	vy :: Int,
	scrx :: Int,
	stand :: Bool,

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

	lr = 1,
	pat = 0,
	anm = 0
	}


patStop = 0
patWalk = 1
walkPatNum = 3
patJump = patWalk + walkPatNum

imgTable = [
	[ImgNario00, ImgNario01, ImgNario02, ImgNario03, ImgNario04],
	[ImgNario10, ImgNario11, ImgNario12, ImgNario13, ImgNario14]
	]


cellCrd :: Int -> Int
cellCrd x = x `div` (chrSize * one)


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
			| (stand player)	= friction (vx player) acc
			| otherwise			= friction (vx player) (acc `div` 2)
		x' = max xmin $ (x player) + vx'
		scrx'
			| vx' > 0 && (x' - (scrx player)) `div` one > 160	= (scrx player) + vx'
			| otherwise											= (scrx player)

		padl = if isPressed (kp PadL) then 1 else 0
		padr = if isPressed (kp PadR) then 1 else 0
		maxspd
			| not $ stand player	= maxVx `div` 2
			| isPressed (kp PadB)	= maxVx * 2
			| otherwise				= maxVx
		xmin = (scrx player) + chrSize `div` 2 * one

		player' = player { x = x', vx = vx', scrx = scrx' }

		lr' =
			case (-padl + padr) of
				0	-> lr player
				-1	-> 0
				1	-> 1
		pat'
			| vx' == 0		= patStop
			| otherwise		= (anm' `div` anmCnt) + patWalk
		anm'
			| vx' == 0		= 0
			| otherwise		= ((anm player) + (abs vx')) `mod` (walkPatNum * anmCnt)
		anmCnt = maxVx * 3


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
				cx = cellCrd (x player + dx * chrSize `div` 2 * one)
				cy = cellCrd (y player - chrSize `div` 2 * one)


-- 重力による落下
fall :: Player -> Player
fall player
	| stand player	= player
	| otherwise		= player { y = y', vy = vy' }
	where
		vy' = min maxVy $ vy player + gravity
		y' = y player + vy'


-- 床をチェック
checkFloor :: Field -> Player -> Player
checkFloor fld player
	| stand'	= player { stand = stand', y = ystand, vy = 0 }
	| otherwise	= player { stand = stand' }
	where
		stand' = isGround $ y player
		ystand = (cellCrd $ y player) * (chrSize * one)

		isGround y = isBlock $ fieldRef fld (cellCrd $ x player) (cellCrd y)


-- 上をチェック
checkCeil :: Field -> Player -> Player
checkCeil fld player
	| stand player || vy player >= 0 || not isCeil	= player
	| otherwise = player { vy = 0 }
	where
		ytmp = y player - one * chrSize

		isCeil = isBlock $ fieldRef fld (cellCrd $ x player) (cellCrd ytmp)
		yground y = (cellCrd y) * (chrSize * one)


-- ジャンプする？
doJump :: KeyProc -> Player -> Player
doJump kp player
	| stand player && kp PadA == Pushed	= player { vy = jumpVy, stand = False, pat = patJump }
	| otherwise							= player


-- 更新処理
updatePlayer :: KeyProc -> Field -> Player -> Player
updatePlayer kp fld player =
	moveY $ checkX fld $ moveX kp player
	where
		moveY
			| stand player	= doJump kp . checkFloor fld . fall
			| otherwise		= checkCeil fld . checkFloor fld . fall

-- スクロール位置取得
getScrollPos :: Player -> Int
getScrollPos player = (scrx player) `div` one

-- 描画
renderPlayer sur imgres scrx player = do
	blitSurface (getImageSurface imgres imgtype) Nothing sur pos
	where
		pos = pt ((x player) `div` one - chrSize `div` 2 - scrx) ((y player) `div` one - chrSize)
		imgtype = imgTable !! (lr player) !! (pat player)
