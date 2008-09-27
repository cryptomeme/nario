module Const where


-- 固定小数点での１
one = 256 :: Int

-- 1キャラのサイズ
chrSize = 16 :: Int

-- 重力
gravity = one `div` 2

-- 画像
data ImageType =
	  ImgNario00 | ImgNario01 | ImgNario02 | ImgNario03 | ImgNario04
	| ImgNario10 | ImgNario11 | ImgNario12 | ImgNario13 | ImgNario14
	| ImgBlock1 | ImgBlock2 | ImgBlock3 | ImgBlock4 | ImgBlock5
	| ImgMt02 | ImgMt11 | ImgMt12 | ImgMt13 | ImgMt22
	| ImgCloud00 | ImgCloud01 | ImgCloud02 | ImgCloud10 | ImgCloud11 | ImgCloud12
	| ImgDk00 | ImgDk01 | ImgDk10 | ImgDk11
	| ImgGrass00 | ImgGrass01 | ImgGrass02
	deriving Eq


imageFn ImgNario00 = "nario00.bmp"
imageFn ImgNario01 = "nario01.bmp"
imageFn ImgNario02 = "nario02.bmp"
imageFn ImgNario03 = "nario03.bmp"
imageFn ImgNario04 = "nario04.bmp"
imageFn ImgNario10 = "nario10.bmp"
imageFn ImgNario11 = "nario11.bmp"
imageFn ImgNario12 = "nario12.bmp"
imageFn ImgNario13 = "nario13.bmp"
imageFn ImgNario14 = "nario14.bmp"
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
imageFn ImgGrass00 = "grass00.bmp"
imageFn ImgGrass01 = "grass01.bmp"
imageFn ImgGrass02 = "grass02.bmp"

images = [
	ImgNario00, ImgNario01, ImgNario02, ImgNario03, ImgNario04,
	ImgNario10, ImgNario11, ImgNario12, ImgNario13, ImgNario14,
	ImgBlock1, ImgBlock2, ImgBlock3, ImgBlock4, ImgBlock5,
	ImgMt02, ImgMt11, ImgMt12, ImgMt13, ImgMt22,
	ImgCloud00, ImgCloud01, ImgCloud02, ImgCloud10, ImgCloud11, ImgCloud12,
	ImgDk00, ImgDk01, ImgDk10, ImgDk11,
	ImgGrass00, ImgGrass01, ImgGrass02
	]
