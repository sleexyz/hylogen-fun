{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module ICLC2016 where
import Util

output = toProgram $ vec4 (v, v, v, 1)
  & mix (osc7) (bbF (texture2D backBuffer) uvN)

bbF x = x
  & lmap (view norm)
  & lmap (rot (w_ audio * 10 + 2 * pi * osc3))
  & lmap (mirrorX)
  -- & lmap (mirrorY)
  & lmap (+(vec2(0, 0.1)))
  -- & lmap (*(0.5 + copy osc2))
  & rgbF 0.01
  & rmap (desat . desat)

v = vqF vq uvN
vqF x = x
  & lmap (rot (w_ audio * 10))
  & lmap (mirrorY)
  & lmap (mirrorX)
  & lmap sin
  & lmap (*(10 * copy osc1))

vq uv = cos (y_ uv * x_ audio * osc0 * 100) * 2 + cos (x_ uv * 5 + beat) / 5
