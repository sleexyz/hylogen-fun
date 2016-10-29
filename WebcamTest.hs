module WebcamTest where

import Util
import Control.Arrow


distribute :: (Vec1 -> Vec1) -> Vec1 -> Vec1
distribute f x = foldr (\a b -> f (x * 2**a) + b) x [0, 1, 2]

output = toProgram $ vec4 (v, v, v, 1)
  & mix 0 (queryTransformer (texture2D channel2) uvN)

queryTransformer :: (Vec2 -> Vec4) -> Vec2 -> Vec4
queryTransformer x = x
  & lmap (view norm)
  -- & lmap (^*(1 - x_ audio))
  -- & rgbF ((sin (rand time)) * 0.02)
  & lmap (\x -> g $ vec2(x_ x, y_ x))

g x = x
  & (\x -> x - rep 0.1 x)

film x = x
  & (+time)
  & (rep (1))

v = gate 0 0.1 (f $ y_ uvN + time/2)

f x = x
  & (\x -> x * y_ uvN * 1)
  & (rep 0.5)
