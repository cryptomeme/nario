-- Bitmap font
module Font (
	fontPut,
	fontPutc
) where

import Multimedia.SDL
import Control.Monad
import Data.Char

fontWidth = 8
fontHeight = 8
fontXN = 16

fontPut sur imgsur x y str = zipWithM_ (\i c -> fontPutc sur imgsur i y c) [x..] str

fontPutc sur imgsur x y c = do
	blitSurface imgsur (Just rc) sur pos
	where
		pos = pt (x * fontWidth) (y * fontHeight)
		ic = ord c - ord ' '
		u = (ic `mod` fontXN) * fontWidth
		v = (ic `div` fontXN) * fontHeight
		rc = Rect u v fontWidth fontHeight
