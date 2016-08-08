{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Poop2 where

import Hylogen.WithHylide

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

mirrorX v = vec2 (abs (x_ v), y_ v)
mirrorY v = vec2 (x_ v, abs(y_ v))




output = toProgram $ vec4 (r, g, b, 0.01) + bb
  where
    r = v
      & (sin)
      & (*10)
    g = v
    b = v
    v =  (sin (atan ((y_ uvN' / x_ uvN' - x_ audio * len uvN')) / x_ uvN' - w_ audio)) + z_ audio
    uvN' = uvN
      -- & (\x -> x - rep (copy $ w_ audio) x)
      & (^*(y_ audio * 5))
      & mirrorX
      & mirrorY
      & (+(negate (mouse)))
    bb = texture2D backBuffer bbN
    bbN = uvN
      -- & (\x -> x * rep (copy $ x_ audio) x)
      & (*0.99)
      & mirrorX
      & mirrorY
      -- & rot time
      & (\x -> x * 0.5 + 0.5)


