module Const where


-- 固定小数点での１
one = 256 :: Int

-- 1キャラのサイズ
chrSize = 16 :: Int


-- 画像
data ImageType =
		ImgNario00 | ImgNario01 | ImgNario02 | ImgNario03 | ImgNario04
	|	ImgNario10 | ImgNario11 | ImgNario12 | ImgNario13 | ImgNario14
	|	ImgBlock1 | ImgBlock2 | ImgBlock3 | ImgBlock4 | ImgBlock5
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

images = [
	ImgNario00, ImgNario01, ImgNario02, ImgNario03, ImgNario04,
	ImgNario10, ImgNario11, ImgNario12, ImgNario13, ImgNario14,
	ImgBlock1, ImgBlock2, ImgBlock3, ImgBlock4, ImgBlock5
	]
