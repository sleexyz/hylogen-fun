{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Jam_2016_10_07 where

import Hylogen.WithHylide
import Data.Profunctor

type Optic p s t a b = p s t -> p a b
type Optic' p s t = p s t -> p s t
type Iso s t a b = forall p. Profunctor p => p a b -> p s t
type Iso' a b = Iso a b a b
type Auto a = Iso' a a



rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )

norm :: (Floating a) => Auto a
norm = dimap (\x -> x * 0.5 + 0.5) (\x -> x * 2 - 1)

hsv :: Optic' (->) (Vec4) (Vec4)
hsv = dimap rgb2hsv hsv2rgb

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

phi uv = atan (y_ uv/x_ uv)

mirrorX :: Vec2 -> Vec2
mirrorX v = vec2 (abs $ x_ v, y_ v)

mirrorY :: Vec2 -> Vec2
mirrorY v = vec2 (x_ v, abs $ y_ v)

wiggle :: Vec2 -> Vec2
wiggle = rot (cos (time * 0.8) * 0.1)

output = toProgram $ rgba
  & mix (2) (bbF (texture2D backBuffer) uvN)


bb = bbF (texture2D backBuffer) uvN
bbF :: Optic' (->) Vec2 Vec4
bbF x = x
  & rmap (let ceff x =  x
                & (\v -> vec4 (y_ uvN + x_ v, y_ v + 0.5, z_ v, w_ v))
          in hsv ceff
         )
  & lmap (\x -> x * 0.5 + 0.5)
  & lmap (+mouse)
  & lmap wiggle
  & lmap mirrorY
  & lmap mirrorX
  & lmap (\x -> x + rep 5 x)


rgba = vec4 (v, v, v, 1)

v :: Vec1
v = vQ len uvN


vQ :: Optic' (->) Vec2 Vec1
vQ x =  x
