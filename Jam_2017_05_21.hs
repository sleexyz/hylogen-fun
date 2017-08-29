{-# LANGUAGE GADTs #-}

module Jam_2017_05_21 where

import Util

output = toProgram $ bb
  where
    bb = bbF (texture2D backBuffer) uvN

bbF x = x
  & lmap (view norm)
  & rgbF 0.01
  & rmap (mix (0.5) rgba)
  & rmap (hsv (+ (copy osc4)))
  & lmap (rot (less id time))
  & lmap (mirrorX)
  & lmap (mirrorY)

rgba = vec4(v, v, v, 1)
  where
    v = gate (0.5) (fract (y_ uvN )) * (sin (x_ uvN * 10) * 0.1)
