
{-# LANGUAGE GADTs #-}

module Jam_2017_08_28 where

import Util

output = toProgram $ bb
  where
    bb = bbF (texture2D backBuffer) uvN

bbF x = x
  & lmap (view norm)
  & rgbF 0.1
  & rmap (mix (osc0) rgba)
  & rmap (hsv (+ (copy osc4)))
  -- & rmap (modY (*0.025))
  & lmap (rot (muchless id time))
  & lmap (rot (pi ))
  & lmap (mirrorX)
  & lmap (mirrorY)

rgba = vec4(v, v, v, 1)
  where
    v = a * b
    a = gate (osc1) ((y_ uvN * x_ uvN) / (100 + y_ uvN )) 
    b = (sin (y_ uvN) + sin (len uvN * 2) * 1.0)
