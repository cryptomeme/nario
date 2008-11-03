-- Player (nario)

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

--import Multimedia.SDL (blitSurface, pt)
import Data.Bits ((.&.))

import Util
import AppUtil (KeyProc, padPressed, padPressing, PadBtn(..), cellCrd, KeyState(..), getImageSurface, Rect(..), putimg)
import Const
import Images
import Field
import Event
import Actor (ActorWrapper(..))
import Actor.Shot
import Mixer


walkVx = one * 4 `div` 2
runVx = one * 11 `div` 4
acc = one `div` 32
acc2 = one `div` 14
jumpVy = -12 * gravity
jumpVy2 = -13 * gravity
scrollMinX = 5 * chrSize + 6
scrollMaxX = 8 * chrSize
gravity2 = one `div` 6		-- Aを長押ししたときの重力
stampVy = -8 * gravity
undeadFrame = frameRate * 2

-- Type of player
data PlayerType = SmallNario | SuperNario | FireNario
	deriving (Eq)

-- State
data PlayerState = Normal | Dead
	deriving (Eq)

-- Structure
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


-- Move horizontal
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

-- Check wall for moving horizontal direction
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

-- Adjust horizontal scroll position
scroll :: Player -> Player -> Player
scroll opl self = self { scrx = scrx' }
	where
		odx = x opl `div` one - scrx opl
		dx = (max 0 $ vx self) * (scrollMaxX - scrollMinX) `div` runVx + scrollMinX
		scrx'
			| d > 0		= scrx self + d
			| otherwise	= scrx self
		d = x self `div` one - scrx self - (max odx dx)


-- Fall by gravity
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


-- Check for stand on a floor
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

-- Check upper wall
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

-- Do jump?
doJump :: KeyProc -> Player -> (Player, [Event])
doJump kp self
	| stand self && padPressed kp PadA	= (self { vy = vy', stand = False, pat = patJump }, [EvSound SndJump])
	| otherwise							= (self, [])
	where
		vy' = (jumpVy2 - jumpVy) * (abs $ vx self) `div` runVx + jumpVy


-- Do shot?
shot :: KeyProc -> Player -> (Player, [Event])
shot kp self
	| canShot && padPressed kp PadB	= (shotPl, shotEv)
	| otherwise						= (self, [])
	where
		canShot = pltype self == FireNario
		shotPl = self { pat = patShot }
		shotEv = [	EvAddActor $ ActorWrapper $ newShot (x self) (y self) (lr self),
					EvSound SndShot
				]


-- Update
updatePlayer :: KeyProc -> Field -> Player -> (Player, [Event])
updatePlayer kp fld self =
	case plstate self of
		Normal	-> updateNormal kp fld self'
		Dead	-> updateDead kp fld self'
	where
		self' = decUndead self
		decUndead pl = pl { undeadCount = max 0 $ undeadCount pl - 1 }

-- In normal state
updateNormal :: KeyProc -> Field -> Player -> (Player, [Event])
updateNormal kp fld self = (self3, ev1 ++ ev2 ++ ev3)
	where
		(self1, ev1) = moveY $ scroll self $ checkX fld $ moveX kp self
		(self2, ev2) = checkCeil fld self1
		(self3, ev3) = shot kp self2
		moveY = doJump kp . checkFloor fld . fall (padPressing kp PadA)

-- In dead state
updateDead :: KeyProc -> Field -> Player -> (Player, [Event])
updateDead kp fld self = (fall False self, [])

-- Get scroll position
getScrollPos :: Player -> Int
getScrollPos = scrx

-- Get x position
getPlayerX :: Player -> Int
getPlayerX = x

-- Get Y position
getPlayerY :: Player -> Int
getPlayerY = y

-- Get y velocity
getPlayerVY :: Player -> Int
getPlayerVY = vy

-- Get hit rect
getPlayerHitRect :: Player -> Rect
getPlayerHitRect self = Rect (xx - 6) (yy - 16) (xx + 6) yy
	where
		xx = x self `div` one
		yy = y self `div` one

-- Get coin num
getPlayerCoin :: Player -> Int
getPlayerCoin = coin

-- Get score
getPlayerScore :: Player -> Int
getPlayerScore = score

-- Get type
getPlayerType :: Player -> PlayerType
getPlayerType = pltype

-- Set type
setPlayerType :: PlayerType -> Player -> Player
setPlayerType t self = self { pltype = t }

-- Set damage
setPlayerDamage :: Player -> Player
setPlayerDamage self
	| undeadCount self > 0			= self
	| pltype self == SmallNario		= self { plstate = Dead, pat = patDead, vy = jumpVy, stand = False }
	| otherwise						= self { pltype = SmallNario, undeadCount = undeadFrame }

-- Stamp enemy
stampPlayer :: Player -> Player
stampPlayer self = self { vy = stampVy }

-- Player get a coin
playerGetCoin :: Player -> Player
playerGetCoin self = self { coin = (coin self + 1) `mod` 100 }

-- Add score
addScore :: Int -> Player -> Player
addScore a self = self { score = score self + a }

-- Render
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
