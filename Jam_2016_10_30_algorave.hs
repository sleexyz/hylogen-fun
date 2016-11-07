{-# LANGUAGE GADTs #-}
module Jam_2016_10_30_algorave where

import Util

output = toProgram $ vec4 (v, v, v, 1)
 & (+ (bbF (texture2D backBuffer) uvN))

bbF f = f
  & lmap (view norm)
  -- & lmap (rot (pi/3))
  -- & lmap (+ mouse)
  -- & lmap (mirrorX)
  -- & lmap (mirrorY)
  & rgbF 0.01
  & rmap (hsv . modY $ (subtract 0.9))
  & rmap (hsv . modZ $ (subtract osc5))
  & rmap (bpf (sin (less id time)) (0.5))
  & rmap (*copy osc7)

v = vF uvN
vF x = x
  & (persp (osc0 * x_ audio * 1) (osc1 + 3 * w_ audio) )
  -- & mirrorX
  -- & mirrorY
  & (* (10* (1 -  x_ audio & copy)))
  & (rot (less id time))
  -- & (+ (vec2 (0, less id time)))
  -- & (rep 2)
  & (cos)
  & (1-)
  & f
  & (*x_ audio)
  & (*osc6)
  & (clamp 0 1)

f x = 0
  -- + x_ x
  -- + y_ x 
  -- - len x * w_ audio * 1 
  -- + (x_$ gate (copy osc3) x)
  + (x_$ gate (copy osc3) x)
  


