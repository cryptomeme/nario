{-# INCLUDE <SDL.h> #-}
{-# OPTIONS -fglasgow-exts #-}

module SDLUtil (blitSurface2) where

import Multimedia.SDL

import Foreign (new, free, Ptr, nullPtr)


blitSurface2 :: Surface -> Maybe Rect -> Surface -> Rect -> IO Int
blitSurface2 src sr dest dr = do
  psr <- case sr of Nothing -> return nullPtr
                    Just sr -> new sr
  pdr <- new dr
  ret <- inSDLBlitSurface (surfaceToPtr src) psr (surfaceToPtr dest) pdr
  free psr
  free pdr
  return ret

--------

foreign import ccall "SDL.h SDL_UpperBlit"        inSDLBlitSurface      :: Ptr () -> Ptr Rect -> Ptr () -> Ptr Rect -> IO Int
