
module Field (
	Field,
	getField,
	fieldRef,
	isBlock,
	renderField
) where

import Multimedia.SDL

import Const
import Util

type Cell = Char
type Field = [[Cell]]


-- マップ

fieldMap :: Field
fieldMap = [
	"                ",
	"                ",
	"                ",
	"                ",
	"                ",
	"                ",
	"        O?O     ",
	"                ",
	"                ",
	"       O?O?O    ",
	"                ",
	"                ",
	"                ",
	"@@@@@@@@@@@@@@@@",
	"@@@@@@@@@@@@@@@@"
	]


getField :: Int -> Field
getField stage = fieldMap

chr2img '@' = ImgBlock1
chr2img 'O' = ImgBlock2
chr2img '?' = ImgBlock4


isBlock :: Cell -> Bool
isBlock c = c `elem` "@O?"

inField :: Field -> Int -> Int -> Bool
inField fld x y = 0 <= y && y < length fld && 0 <= x && x < length (fld !! y)

fieldRef :: Field -> Int -> Int -> Cell
fieldRef fld x y
	| inField fld x y	= fld !! y !! x
	| otherwise			= ' '


renderField sur imgres = sequence_ $ concatMap lineProc $ zip [0..] fieldMap
	where
		lineProc (y, ln) = map (cellProc y) $ zip [0..] ln
		cellProc y (x, c) = do
			if c == ' '
				then return ()
				else do
					blitSurface (getImageSurface imgres $ chr2img c) Nothing sur $ pt (x*16) (y*16)
					return ()

