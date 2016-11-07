{-# LANGUAGE GADTs #-}

module Jam_2016_10_30 where

import Util

output = toProgram $ vec4 (v, v, v, 1) * copy osc6 * copy (1.5 - z_ audio)
  & (+ (bbF (texture2D backBuffer) uvN) * copy osc7)

bbF x = x
  & lmap (view norm)
  -- & lmap (rot (2* pi/3))
  -- & lmap (+ (mouse))
  -- & lmap mirrorX
  -- & lmap mirrorY
  -- & lmap (*0.9)
  & rgbF 0.001
  & rmap (hsv . modY $ (subtract 0.1))
  -- & rmap (hsv . modY $ (+ 0.1))
  & rmap (hsv . modZ $ (subtract osc5))
  & rmap (bpf (x_ audio) 0.9)


v = uvN
  & persp (10 * osc0 * (1 - x_ audio)) (100 * osc1 * (0.2 -  w_ audio ))
  & rot (less id time)
  & (+vec2(0, osc4 * 10 + id time))
  & (*10)
  & (\x -> cos x)
  & f 
  & (*(x_ audio + 0.5))
  & (1 - )
  & clamp 0 1


f x = 0
  + x_ x * cos (beat' + x_ audio)
  + y_ x * sin (beat' + x_ audio)
  - len x
  + x_ (gate g x)
  + y_ (gate g x)
  where
    g = copy osc3


