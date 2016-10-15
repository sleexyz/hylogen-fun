{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Jam_2016_10_10 where

import Util

output = toProgram (remix rgba uvN)


rgba :: Vec2 -> Vec4
rgba uv = bw (val uv)
 -- & mix 0.1 (bbF(texture2D channel1) uv)
 & mix 0.01 (bbF(texture2D backBuffer) uv)

bbF :: Optic' (->) Vec2 Vec4
bbF x = x
  & lmap (view norm)
  & lmap (*(1 + 0.1))
  & lmap (+mouse)
  & lmap (\x -> mix (w_ audio * 10) x (rep 0.1 x))
  & lmap (rot (0.01))
  & lmap mirrorX
  & lmap mirrorY

remix :: Optic' (->) Vec2 Vec4
remix f = f
  & rgbF 0.1
  & rmap (hsv (+0.01))

val :: Vec2 -> Vec1
val = len
  & rmap (*(x_ audio))
  & lmap (norm sin)
  & lmap (*0.9)
