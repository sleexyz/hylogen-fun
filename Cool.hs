{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}

module Audio where

import Hylogen.WithHylide
import Data.VectorSpace
import Data.Function
import Control.Arrow

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )

over :: Vec4 -> Vec4 -> Vec4
over x y = mix (a) x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - w_ x'

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

fresh = color `over` c bb
  where
  c = id
    -- >>> (\x -> vec4(x!Z, x!Y, x! X, x!W))
    -- >>> (**0.95)
  bb = texture2D backBuffer (f uvN)
  color = sel ( g uvN `lt` copy (y_ audio)) (vec4(sin time + 1, 0, 0, 0.90 :: Vec1)) (vec4(0, 0, 0, -0.1))
  g = id
    -- >>> (\x -> rep 2.5 x)
    >>> (\x -> abs x)
  f = id
    >>> (\x -> vec2(abs x_ x, abs(y_ x)))
    -- >>> (\x -> x + vec2 (0, n $audio!Y))
    >>> (\x -> x + mouse)
    -- >>> (rot (audio!X))
    >>> (rot (pi/3))
    -- >>> (\x -> x + vec2 (0, n $audio!Y))
    -- >>> (rot (audio!X))
    >>> (\x -> x^*(1.4 + w_ audio))
    >>> (\x -> x*0.5 + 0.5)
  n = id
    >>> negate

fresh1 = color `over` c bb where
  c = id
    -- >>> (\x -> vec4(x!Z, x!Y, x! X, x!W))
    >>> (**0.95)
  bb = texture2D backBuffer (f uvN)
  color = sel ( g uvN `lt` copy (y_ audio)) (vec4(tan $ 5 * z_ audio, 0, 0, 0.90 :: Vec1)) (vec4(0, 0, 0, 0.01))
  g = id
    >>> (\x -> rep 2.5 x)
    >>> (\x -> abs x)
  f = id
    >>> (\x -> vec2(abs x_ x, abs(y_ x)))
    >>> (\x -> x + vec2 (0, n $y_ audio))
    >>> (rot (x_ audio))
    >>> (\x -> x^*(1.4 + w_ audio))
    >>> (\x -> x*0.5 + 0.5)
  n = id
    >>> negate

fresh2 = color `over` c bb where
  c = id
    -- >>> (\x -> vec4(x!Z, x!Y, x! X, x!W))
    >>> (**0.95)
  bb = texture2D backBuffer (f uvN)
  color = sel ( g uvN `lt` copy (y_ audio)) (vec4(copy . tan . (*5) $ z_ audio, 0.90 :: Vec1)) (vec4(0, 0, 0, 0.01))
  g = id
    >>> (\x -> rep 2.5 x)
    >>> (\x -> abs x)
  f = id
    >>> (\x -> vec2(abs x_ x, abs(x!Y)))
    >>> (\x -> x - mouse)
    >>> (\x -> x^*(x_ audio + 0.5))
    >>> (rot (w_ audio * 20 + pi/2))
    >>> (\x -> x*0.5 + 0.5)
  n = id
    >>> negate

fresh3 = color `over` c bb where
  c = id
    -- >>> (\x -> vec4(x!Z, x!Y, x! X, x!W))
    >>> (**0.95)
  bb = texture2D backBuffer (f uvN)
  color = sel (( len . tan . (*(copy $ y_ audio * 10))  $ uvN) `lt` 1) (vec4(copy . tan . (*5) $z_ audio, 0.90 :: Vec1)) (vec4(0, 0, 0, 0.01))
  g = id
    >>> (\x -> abs x)
  f = id
    >>> (\x -> vec2(abs x_ x, abs(y_ x)))
    >>> (\x -> x - mouse)
    >>> (\x -> x^*(x_ audio + 0.5))
    >>> (rot (w_ audio * 20 + pi/2))
    >>> (\x -> x*0.5 + 0.5)
  n = id
    >>> negate



output = toProgram fresh2
