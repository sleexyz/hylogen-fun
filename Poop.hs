{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Poop where

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

-- v = cos (asin (y_ uvN / x_ uvN) * y_ audio))
-- v = cos (asin (y_ uvN / (sin $ x_ uvN * 10 * x_ audio) + y_ audio))
--

bar = toProgram (vec4 (r v, g v, b v,  0.01) + bb)
  where
    fun =  negate . (tanh)
    r = fun . (* (z_ bb)) 
    g = fun .(* (x_ bb))
    b = fun . (* (1) )
    v = clamp 0 1 (cos (atan (y_ uvN' / (cos $ x_ uvN' * 20 * x_ audio)/y_ uvN + y_ audio))) - x_ audio
      where
        uvN' = uvN
          & mirrorX
          & mirrorY
          & rot (sin ( y_ audio * 2.5) - y_ uvN)
    bb = texture2D backBuffer (f uvN)
      where
        f = id
          . (\v -> v * 0.5 + 0.5)
          . (\v -> v ^* (1 + x_ audio * 2))
          . (\v -> rot (w_ audio  * 5.0 * len uvN) v)
          . (\v -> v ^* 0.5)
          . mirrorX
          . mirrorY
          -- . (\v -> v  - mouse)

output = bar

-- foo = toProgram (vec4 (r v, g v, b v, 0.01) + bb)
--   where
--     fun = negate
--     r = (\x -> fun x * 1.0)
--     g = (\x -> fun x * 1.0)
--     b = (\x -> fun x * 1.0)
--     -- v = cos (asin (y_ uvN / x_ uvN) + time)
--     v = clamp 0 1 (cos (asin (y_ uvN / x_ uvN * y_ audio) / x_ uvN )) - x_ audio
--     bb = texture2D backBuffer (f uvN)
--       where
--         f = id
--           . (\v -> v * 0.5 + 0.5)
--           . (\v -> v ^* (1 + x_ audio))
--           . (\v -> rot (w_ audio + time) v)
--           . (\v -> v ^* 1.1)
--           . mirrorX
--           . mirrorY

