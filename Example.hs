{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Example where

import Hylogen.WithHylide
import Data.Profunctor


type Iso s t a b = forall p. Profunctor p => p a b -> p s t
-- dimap f g id == id



shift :: Floating a =>  a -> Iso a a a a
shift x =  dimap (+x) (subtract x)

scale :: Floating a => a -> Iso a a a a
scale x = dimap (*x) (/x)

_linlin :: Floating a => (a, a, a, a) -> Iso a a a a
_linlin (x, y, z, w) = dimap (linlin (x, y, z, w)) (linlin (z, w, x, y))

output :: Program
output = toProgram color

phi :: Vec2 -> Vec1
phi v = atan (y_ v / x_ v)

color :: Vec4
color = vec4 (v, v, v, 1)
  where
  v = (_linlin (0, 1, 0.5, 1)) sin (len uvN)
