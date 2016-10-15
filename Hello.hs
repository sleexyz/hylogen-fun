module Hello where

import Util


output = toProgram rgba

rgba = vec4 (rgb, 1)
  -- & mix (-0.1) bb

bb = texture2D backBuffer (f uvN)
f x = x
  & (subtract mouse)
  & (*1.5)
  & view norm

red = vec3 (1, 0, 0)
rgb :: Vec3
rgb = h + w
h = sin (copy (len uvN) * 20  + copy time) * red
w = sin (copy (len uvN) * 30  + copy (time * 2))
