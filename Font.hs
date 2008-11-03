-- -*- mode: haskell; Encoding: UTF-8 -*-
-- Bitmap font
module Font (
	Font(..),
	fontPut,
	fontPutc
) where

import Multimedia.SDL (blitSurface, pt, Rect(..), Surface)
import Control.Monad (zipWithM_)
import Data.Char (ord)

data Font = Font {
	fontSurface :: Surface,
	fontWidth :: Int,
	fontHeight :: Int,
	fontXN :: Int
	}


-- Put string
fontPut font sur x y str = zipWithM_ (\i c -> fontPutc font sur i y c) [x..] str

-- Put char
fontPutc font sur x y c = do
	blitSurface (fontSurface font) (Just rc) sur pos
	where
		pos = pt (x * (fontWidth font)) (y * (fontHeight font))
		ic = ord c - ord ' '
		u = (ic `mod` xn) * w
		v = (ic `div` xn) * h
		rc = Rect u v w h

		xn = fontXN font
		w = fontWidth font
		h = fontHeight font
