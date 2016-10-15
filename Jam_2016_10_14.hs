{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Jam_2016_10_14 where

import Util
import Spirangle (spirangle)

-- can we generalize this?
toggleOn x g p = mix x p (g p)
toggleOff x g p = mix (1 - x) p (g p)

output = toProgram $ vec4 (0, 0, 0, 0)
  & const (vec4 (v, v, v, 1) )
  & (mix 0.1 (bb))
  -- & (mix (1 - osc1) (spirangle osc1))
  -- & (+ (spirangle osc1))
  -- & (^*(1 - osc0))

bb = bbq (texture2D backBuffer) uvN
bbq x = x
  & lmap (view norm)
  & lmap (rot (less sin time))
  -- & lmap (mirrorY)
  -- & lmap (mirrorX)
  & rgbF 0.1
  & rmap (desat . desat)
  & rmap (modZ (*1.1))


v = q x_ uvN
q x = x
  & lmap (sin)
  & lmap (+ (copy time * 10))
  & rmap (+(x_ uvN))
