{-# LANGUAGE GADTs #-}

module Jam_2016_11_03 where

import Util

output = toProgram $ v

v = vF (texture2D backBuffer) uvN


vF x = x
  & rmap opaque
  & lmap (view norm)
  & lmap (mirrorX)
  & lmap (mirrorY)
  & rgbF 0.01
  & lmap (\x -> rot (x_ x * 1 + muchless sin time) x )
  & lmap (mirrorX)
  & speckle (rep 0.05)

speckle a f x = rmap g f x
  where
    g = modV (\v -> v + (a x & x_ ))
