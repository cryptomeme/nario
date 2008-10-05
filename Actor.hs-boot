{-# OPTIONS_GHC -fglasgow-exts #-}

module Actor where

class Actor a where

data ActorWrapper = forall a. Actor a => ActorWrapper a
