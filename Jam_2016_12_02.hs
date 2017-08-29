{-# LANGUAGE GADTs #-}
module Jam_2016_12_02 where

import Util

output = toProgram $ vec4 (v, v, v, 0.1)
 -- & (const  (bbF1 (texture2D channel2) uvN * 0.1))
 & (+ (bbF2 1 (texture2D backBuffer) uvN * 0.9))
 -- & (+ (bbF2 2 (texture2D backBuffer) uvN * 0.45))
 -- & (+ (bbF2 4 (texture2D backBuffer) uvN * 0.3))


bbF1 f = f
  & lmap (view norm)
  & lmap (* vec2 (-1, 1))
  & lmap (\x -> x + rep 0.5 x)
  & lmap (subtract (mouse))
  -- & lmap (^* (1 - y_ audio * 0.01))
  -- & lmap (mirrorX)
  -- & lmap (rot (less id time))
  -- & lmap (persp (sin (less id time)) (less cos time + (-2) + 2 * x_ audio))
  -- & lmap (mirrorY)
  -- & lmap (mirrorX)
  -- & rmap (hsv . modZ $ (*5))
  -- & rmap (hsv . modX $ (*100))
  -- & rmap (bpf (sin (less id time)) (0.1))

bbF2 v f = f
  & lmap (view norm)
  -- & lmap (\x -> x / (rep 0.01 x))
  -- & lmap (mirrorX)
  -- & lmap (mirrorY)
  -- & lmap (subtract  (mouse))
  & lmap (\x -> x + rep 0.5 x)
  & lmap (* v)
  -- & rgbF 0.01
  -- & lmap (rot (2 * pi/6))
  -- & lmap (mirrorX)
  -- & rmap (hsv . modZ $ (*0.9))
  -- & rmap (hsv . modZ $ (subtract 0.1))
  -- & lmap (mirrorY)
  -- & rgbF 0.01
  -- & rmap (bpf (sin (less id time)) (0.5))

v = vFF vF uvN
vF x = len x
  -- & (* (x_ audio))
  & (\x -> x - rep 0.5 x)

vFF x = x
  & lmap (subtract mouse)
  -- & lmap (persp 1 1)
  & lmap (mirrorY)
  & lmap (mirrorX)
