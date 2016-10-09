{-# LANGUAGE GADTs #-}

module Jam_2016_06_14 where

import Hylogen.WithHylide
import Hylogen.Expr

output = toProgram noisyink

(?) :: (ToGLSLType a) => Booly -> (Expr a, Expr a) -> Expr a
b ? (x, y) = sel b x y
(<&>) = flip fmap

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )
phi x = atan (y_ x/x_ x)

over :: Vec4 -> Vec4 -> Vec4
over x y = mix (a) x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - w_ x'


spherething = vec4 (v, v, v, 1)
  where
    v = product $ do
      x <- fromInteger <$> [1..5]
      return $ atan (abs (x_ uvN * len uvN) + x * y_ uvN) * 100+ tan (len uvN*4 + time)

thanger1 = vec4 (v, v, v, 1)
  where
    v = product $ do
      x <- [1..20] <&> fromInteger <&> (/5)
      return $ atan (abs (x_ uvN * len uvN) + x * y_ uvN * tan time) * 100 
             -- + tan (len uvN*4 + time)

thanger2 = vec4 (v, v, v, 1)
  where
    v = product $ do
      x <- [1..10] <&> fromInteger <&> (*5)
      return $ 0
             + atan (abs (x_ uvN * len uvN ) + x * ((y_ uvN ) / 4) * (x_ audio * 2 - 1) ) * 10
             + tan (y_ uvN * 100 + time) ** (z_ audio * 4)
             - tan (x_ uvN * 100 + time) ** (z_ audio * 4)

thanger3 = vec4 (v, v, v, 1)
  where
    v =  mix v' v' 0
    v' = product $ do
      x <- [1..10] <&> fromInteger <&> (*0)

      return $ 0
             + atan (abs (x_ uvN * len uvN )) * 10
             + (len uvN * x_ audio) * 10
             + tan (y_ uvN * 500 + time) * (y_ audio)
             - tan (x_ uvN * 500 + time) * (y_ audio) 
             


thanger4 = vec4 (v, v, v, 1)
  where
    v = sum $ do
      x <- [1..60] <&> fromInteger

      let v' =  clamp 0 1 (mix m square 0)
          ds n = (/n) . floor_ . (*n)
          t = ((fract time `lt` 0.5) ? (ds 10 time, time)) + x * y_ mouse
          m = (fract t `lt`  0.1)  ? (0, 1)
          pos = uvN  + rot t 0.5
          square = sel (abs pos `lt` 0.01) 1 0 

      return v'

thanger5 = vec4 (v, v, v, 1)
  where
    v = sum $ do
      x <- [1] <&> fromInteger

      let v' =  sel (abs pos `lt` 0.1) (sin t) (1 - sin t)
          ds n = (/n) . floor_ . (*n)
          t = ((fract t' `lt` 0.5) ? (ds 2 t', t')) / m
            where
              t' = time * m
              m = 20
          pos = uvN  + rot (cos t) 0.5

      return v'

notstarfield = vec4 (v, v, v, 1)
  where
    v = 1 - len uvN * cos (atan (y_ uvN / x_ uvN ) * 10)

notstarfield1 = vec4 (v, v, v, 1)
  where
    v = 1 - len uvN * cos (atan (y_ uvN / x_ uvN ) * 10000)

notstarfield2 = vec4 (v, v, v, 1)
  where
    v = len uvN - cos (atan (y_ uvN / x_ uvN ) *  sin time * 5)




sassyballsack = vec4 (v, v, v, 1)
  where
    v = 1 - len (uvN  * 4) + cos (phi (uvN + off))
    off = rot (time * 3) 0.05 - vec2 (0, 0.3)


weird = vec4 (v, v, v, 1)
  where
    v = 2 - len (uvN  * 2) 
      + sin ((phi .abs . sin . (*5) $ uvN)  + time)
      - cos ((phi . abs . cos . (*10) $ uvN)  + time)

godthing = vec4 (v, v, v, 1)
  where
    cart v = vec2 (cos v, sin v)
    v = 1 - len ((uvN * cart (phi (uvN) * 20 + copy time)))  * 10

shittystarfield = vec4 (v, v, v, 1)
  where
    cart v = vec2 (cos v, sin v)
    v = sum $ do
      x <- fromInteger <$> [1..20]

      return . star $ vec3(sin (x * 135), cos (x * 137 + time), 10)

    star v = clamp 0 1 (1 - len (pos * cart (phi (pos) * 10)) * dist) /dist * 10
      where
        pos = uvN - xy_ v
        dist = z_ v

noisyink0 = vec4 (v, v, v, 1)
  where
    cart v = vec2 (cos v, sin v)
    v = sum $ do
      let t = time * 0.1
      x <- fromInteger <$> [1..20]

      return . star $ vec3(sin (x * 235 + t) * 1, cos (x * 239 + t) * 3, sin x)

    star v = clamp 0 1 (1 - len (pos * cart (phi (pos) * 50)) * dist) /dist * 10
      where
        pos = uvN - xy_ v
        dist = z_ v

noisyink1 = vec4 (v, v, v, 1)
  where
    cart v = vec2 (cos v, sin v)
    v = sum $ do
      let t = time * 0.01
      x <- fromInteger <$> [1..20]

      return . star $ vec3(sin (x * 283 + t) * 1, cos (x * 293 + t) * 5, sin x)

    star v = clamp 0 1 (1 - len (pos * cart (phi (pos) * 100)) * dist) /dist
      where
        pos = uvN - xy_ v
        dist = z_ v 

noisyink = vec4 (r v, g v, b v, 1)
  where
    [r, g, b] = (\x y -> atan (cos (x ) + y)) <$> [653, 659, 661]
    cart v = vec2 (cos v, sin v)
    v = sum $ do
      let t = time * 0.01
      x <- fromInteger <$> [1..20]

      return . star $ vec3(sin (x * 283 + t) * 1, cos (x * 293 + t) * 5, sin x)

    star v = clamp 0 1 (1 - len (pos * cart (phi (pos) * 100)) * dist) /dist
      where
        pos = uvN - xy_ v
        dist = z_ v 
