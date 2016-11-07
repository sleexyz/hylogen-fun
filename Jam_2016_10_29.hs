{-# LANGUAGE GADTs #-}
module Jam_2016_10_29 where

import Util

output = toProgram $ vec4 (v, v, v, 1) 
  & (+(bbF (texture2D backBuffer) uvN * copy (osc7 + 0.7 - x_ audio)))
bbF x = x
  & lmap (view norm)
  & rgbF 0.01
  & rmap (hsv . modY $ (subtract 0.9))
  & rmap (hsv . modZ $ (subtract 0.5))

v = uvN
  & (persp (osc0 * 100 * (1 - x_ audio)) (osc1 * 100 * w_ audio))
  & (rot (less id time))
  & (subtract $  vec2(0, less id $ time))
  & (rep (copy osc4 * 0.5))
  & (^* (x_ audio * 100))
  & tan
  & (gate (copy $ x_ audio ))
  & y_
  & (*osc6)

