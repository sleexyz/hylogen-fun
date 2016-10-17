{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Jam_2016_10_14 where

import Util


output = toProgram $ vec4 (v, v, v, 1)
  & mix 0.1 bb

bb = bbqF (texture2D backBuffer) uvN
bbqF x = x
  & lmap (view norm)
  & lmap (*0.9)
  & rgbF 0.1
  & rmap desat & rmap desat & rmap desat & rmap desat
  & rmap (bpf 0.7 0.1)

v = vqF vq uvN

vqF x = x
  & lmap (\x -> x - rep 0.5 x)
  & lmap (\x -> x - rep 0.5 x)
  & lmap (cos)
  & lmap (*(10))
  & lmap (+(vec2 (0, less id time)))
  & lmap (clamp (-1) 1)
  -- & lmap (^* y_ audio)

vq uv = y_ uv * 2  + sin (x_ uv + time)






