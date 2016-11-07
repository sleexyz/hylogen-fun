{-# LANGUAGE GADTs #-}
module Perspective1 where

import Util
import Spirangle

output = toProgram $ vec4 (v, v, v, 1) 
 & (+ (bbF (texture2D backBuffer) uvN) * copy osc7)
 & (+ (spirangle (gate 1 beat) * copy osc5 ))

bbF x = x
 & lmap (view norm)
 & lmap (rot (osc3 * 2 * pi * tan (2 * beat)))
 & lmap (subtract mouse)
 & lmap (mirrorX)
 & rgbF 0.01
 & rmap (hsv . modX $ (subtract (sin (less id time))))
 & rmap (hsv . modY $ (subtract 0.1))


v = vq uvN

vq uv =  uv
 & (subtract mouse)
 & (\x -> persp (osc0 * 10 * w_ audio) (x_ audio * 10 * osc1) x)
 & rot (tan  $ beat * pi)
 & rep (0.5)
 & gate 0.1
 & len
 & (*osc6)
 & (clamp 0 1)

