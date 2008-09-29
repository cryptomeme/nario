module Const where


-- 固定小数点での１
one = 256 :: Int

-- 1キャラのサイズ
chrSize = 16 :: Int

-- 重力
gravity = one `div` 2
gravity2 = one `div` 4

-- 画像
data ImageType =
	  ImgNarioLStand | ImgNarioLWalk1 | ImgNarioLWalk2 | ImgNarioLWalk3 | ImgNarioLJump | ImgNarioLSlip
	| ImgNarioRStand | ImgNarioRWalk1 | ImgNarioRWalk2 | ImgNarioRWalk3 | ImgNarioRJump | ImgNarioRSlip
	| ImgBlock1 | ImgBlock2 | ImgBlock3 | ImgBlock4 | ImgBlock5
	| ImgMt02 | ImgMt11 | ImgMt12 | ImgMt13 | ImgMt22
	| ImgCloud00 | ImgCloud01 | ImgCloud02 | ImgCloud10 | ImgCloud11 | ImgCloud12
	| ImgDk00 | ImgDk01 | ImgDk10 | ImgDk11
	| ImgGrass0 | ImgGrass1 | ImgGrass2
	| ImgPole0 | ImgPole1
	| ImgFont
	| ImgTitle
	deriving Eq


imageFn ImgNarioLStand = "narioLStand.bmp"
imageFn ImgNarioLWalk1 = "narioLWalk1.bmp"
imageFn ImgNarioLWalk2 = "narioLWalk2.bmp"
imageFn ImgNarioLWalk3 = "narioLWalk3.bmp"
imageFn ImgNarioLJump = "narioLJump.bmp"
imageFn ImgNarioLSlip = "narioLSlip.bmp"
imageFn ImgNarioRStand = "narioRStand.bmp"
imageFn ImgNarioRWalk1 = "narioRWalk1.bmp"
imageFn ImgNarioRWalk2 = "narioRWalk2.bmp"
imageFn ImgNarioRWalk3 = "narioRWalk3.bmp"
imageFn ImgNarioRJump = "narioRJump.bmp"
imageFn ImgNarioRSlip = "narioRSlip.bmp"
imageFn ImgBlock1 = "block1.bmp"
imageFn ImgBlock2 = "block2.bmp"
imageFn ImgBlock3 = "block3.bmp"
imageFn ImgBlock4 = "block4.bmp"
imageFn ImgBlock5 = "block5.bmp"
imageFn ImgMt02 = "mt02.bmp"
imageFn ImgMt11 = "mt11.bmp"
imageFn ImgMt12 = "mt12.bmp"
imageFn ImgMt13 = "mt13.bmp"
imageFn ImgMt22 = "mt22.bmp"
imageFn ImgCloud00 = "cloud00.bmp"
imageFn ImgCloud01 = "cloud01.bmp"
imageFn ImgCloud02 = "cloud02.bmp"
imageFn ImgCloud10 = "cloud10.bmp"
imageFn ImgCloud11 = "cloud11.bmp"
imageFn ImgCloud12 = "cloud12.bmp"
imageFn ImgDk00 = "dk00.bmp"
imageFn ImgDk01 = "dk01.bmp"
imageFn ImgDk10 = "dk10.bmp"
imageFn ImgDk11 = "dk11.bmp"
imageFn ImgGrass0 = "grass0.bmp"
imageFn ImgGrass1 = "grass1.bmp"
imageFn ImgGrass2 = "grass2.bmp"
imageFn ImgPole0 = "pole0.bmp"
imageFn ImgPole1 = "pole1.bmp"
imageFn ImgFont = "font.bmp"
imageFn ImgTitle = "title.bmp"

images = [
	ImgNarioLStand, ImgNarioLWalk1, ImgNarioLWalk2, ImgNarioLWalk3, ImgNarioLJump, ImgNarioLSlip,
	ImgNarioRStand, ImgNarioRWalk1, ImgNarioRWalk2, ImgNarioRWalk3, ImgNarioRJump, ImgNarioRSlip,
	ImgBlock1, ImgBlock2, ImgBlock3, ImgBlock4, ImgBlock5,
	ImgMt02, ImgMt11, ImgMt12, ImgMt13, ImgMt22,
	ImgCloud00, ImgCloud01, ImgCloud02, ImgCloud10, ImgCloud11, ImgCloud12,
	ImgDk00, ImgDk01, ImgDk10, ImgDk11,
	ImgGrass0, ImgGrass1, ImgGrass2,
	ImgPole0, ImgPole1,
	ImgFont,
	ImgTitle
	]
