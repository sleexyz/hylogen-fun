{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Effects where

import Hylogen.WithHylide
import Data.Monoid
import Control.Lens.Wrapped
import Control.Applicative
import Data.Profunctor

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )

rep :: Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

fade :: Vec1 -> Vec4 -> Vec4 -> Vec4
fade a v w = vec4 (a *^ (xyz_ v) + (1 - a) *^ (xyz_ w), (w_ v + w_ w)/2)

inNorm :: Veccable v => (Vec v -> Vec v) -> (Vec v -> Vec v)
inNorm = dimap norm unnorm

norm :: Veccable v => Vec v -> Vec v
norm = (/2) . (+1)

unnorm :: Veccable v => Vec v -> Vec v
unnorm = (`subtract`1) . (*2)

inMiddle :: Veccable v => (Vec v -> Vec v) -> Vec v -> Vec v
inMiddle = dimap (+0.5) (`subtract`0.5)

mirrorY :: Vec2 -> Vec2
mirrorY (Vec2 x y) =  vec2 (inMiddle abs x, y)

mirrorX :: Vec2 -> Vec2
mirrorX (Vec2 x y) =  vec2 (inMiddle abs x, y)


-- (Vec1 -> b) -> (Vec1 -> b)


-- | Filters
rgbF :: Vec1 -> (Vec2 -> Vec4) -> (Vec2 -> Vec4)
rgbF m q pos = vec4 (r, g, b, a)
  where
    r = q (pos + copy m) & x_
    g = q pos & y_
    b = q (pos - copy m) & z_
    a = q pos & w_


pattern Vec2 x y <- (toList -> [x, y])

pattern Vec3 x y z <- (toList -> [x, y, z])
pattern Vec4 x y z w <- (toList -> [x, y, z, w])



chain :: [a -> a] -> a -> a
chain = ala Endo foldMap

-- output = toProgram
-- output1 :: Program
output = toProgram color
  where
    color = shader2 + shader1 * 0.5

    shader1 = chain
      [ id
      , lmap (^/(x_ audio))
      ] (texture2D backBuffer) (uvN & norm)

    eff = ala Endo foldMap 
      [ id
      , lmap unnorm
      , lmap (^/(x_ audio))
      , lmap (rep 2)
      , rgbF (rep (0.05 * x_ audio) time)
      ]
    shader2 = eff (texture2D channel1) $ 
      uvN 
      & (\(Vec2 x y) -> vec2 (x, negate y))
      & norm
    

