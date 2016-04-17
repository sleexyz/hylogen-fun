{-# LANGUAGE NoMonomorphismRestriction #-}
module Test where

import Hylogen
import Data.VectorSpace
import Data.Function

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (X a)
                   + sin phi * (Y a)
                 , (-1) * sin phi * (X a)
                   + cos phi * (Y a)
                 )

radius :: Vec2 -> Vec1
radius uv' = sqrt (X uv' ** 2 + Y uv' ** 2)

sigmoid :: Vec1 -> Vec1
sigmoid x = recip (1 + exp (negate x))

phi uv' = atan (Y uv'/ X uv')





myColor1 = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + W audio
    g = v * Y audio
    b = v * Z audio

    v = cos (radius uvN * X audio * 100 + time)
      + 0.9 *^ tan (X uvN * X audio * 10 + time)
    bb = texture2D backBuffer ((rot (Z audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)



myColor2 = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + W audio
    g = v * Y audio
    b = v * Z audio

    v = cos (radius uvN * X audio * 100 + time)
      + 0.9 *^ sin (X uvN * X audio * 10 + time)
    bb = texture2D backBuffer ((rot (Z audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)






















myColor = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + W audio
    g = v * Y audio
    b = v * Z audio

    v = v' * Z audio
    v' =recip $ tan  (Y uvN * X audio * 10 + tim)
      + 0.9 *^ sin (X bb  * X audio * 10 +tim )
    bb = texture2D backBuffer ((rot (Z audio) (((Y audio *^ uvN)) * 0.9)) * 0.5 + 0.5)
    tim = time * 0.1


myColor3 = 0.1 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v' * W audio
    g = v' * Z audio
    b = v' * Y audio
    v' = product $ map v [0..10]
    v x = tan ((X audio + phi uvN') + time * 0.01 + W audio)
      where
        uvN' = rot time (Y audio *^ vec2 (Y uvN , X uvN) + (Z audio *^ 0.2) * fromInteger (x))
    bb = texture2D backBuffer (0.5 * (rot tim (uvN * 1.1)) + 0.5)
    tim = time * 0.1














mirror :: Vec2 -> Vec2
mirror v = vec2 (abs $ X v, Y v)

myColor4 = 0.01 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v * X uvN
    g = v * Y uvN
    b = v
    v = radius (uvN - mouse) * (10 * X audio)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror

myColor5 = 0.01 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v * X uvN
    g = v * Y uvN
    b = v
    v = radius (uvN - mouse) * (100 * X audio)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror


spaceKandinsky = 0.01 *^ vec4 (r,g ,b, 1) + 1.01 *^ bb
  where
    r = v * fract (X uvN * 100)
    g = v * fract (Y uvN * 100)
    b = v
    v = radius (uvN - mouse) * 10
      & \x -> 10 * sin x
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (pi/2) . (*(0.5)) . mirror

dashiki= 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    mul = 100
    r = v * fract (X uvN * mul)
    g = v * fract (Y uvN * mul)
    b = v
    tim = time/ 10e1
    v = radius (uvN - mouse + vec2 (sin tim, cos tim)) * Y mouse
      & \x -> 10 * sin x
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (pi/4)
          . (*(0.99))

satanic = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    r = v
    g = v
    b = v
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (uvN - mouse + vec2(sin tim, cos tim))
      & (\x -> 10 * x + (fromInteger n))
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (2 * pi / 5)
          . (*(0.99))

cottonCandy = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    g = v * Y audio * X uvN
    b = v * Z audio * fract (radius (uvN * 100))
    r = v * W audio * cos (radius (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (vec2 (X uvN, (-1) * Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * W audio)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

cottonCandy2 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * Y audio * X uvN
    r = 1 - v * Z audio * fract (radius (uvN * 100))
    g = 0.5 + v * W audio * cos (radius (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (vec2 (X uvN, (-1) * Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * W audio)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

cottonCandy3 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * Y audio * X uvN
    r = v * Z audio * fract (radius (uvN * 100))
    g = v * W audio * cos (radius (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (vec2 (X uvN, Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (pi/ 6 * W audio)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

cottonCandy4 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    r = v * X uvN
    g = v * Y uvN
    b = v
    v = tan $ radius (uvN - mouse) * X audio * 100
    bb = texture2D backBuffer (0.5 * fn (uvN) + 0.5)
      where
        fn = id
          . mirror
          . rot (time * 0.001)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

white :: Vec4
white = vec4 (1, 1, 1, 1)

black :: Vec4
black = vec4 (0, 0, 0, 1)

test = select true black white

-- main = putStrLn . toGLSL $ gameOfLife
-- gameOfLife = vec4 (v, v, v, 1)
--   where
--     v = select false 1 0

candyRoad = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 1 -  v * Y audio * X uvN
    r = v * Z audio * fract (radius (uvN * 100))
    g = v * W audio * cos (radius (uvN * 100))
    v = product $ map fn [0..10]
    fn n = Y (vec2 (X uvN, Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 10)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+vec2(0, -0.33))
          . mirror
          . rot (0.3* W audio* m)
          . (^*(X audio & linexp (0, 1, 1.1, 0.9)))
          . (+vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)

candyRoad2 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ bb
  where
    r :: Vec1 -> Vec1
    r v = v - tan (v * 0.001 + sin(tim + Y uvN)) ** 0.1

    g :: Vec1 -> Vec1
    g v = v - tan (v * 0.001 + sin(tim + X uvN)) ** 0.3

    b :: Vec1 -> Vec1
    b v = v - tan (v * 0.001 + sin(tim + radius uvN)) ** 0.4

    tim = time * 0.01

    v = (X audio & linexp (0, 1, 1, 100)) - radius (uvN - vec2 (0.5 * Y audio, -0.333)) * Y audio
      & (*(X audio & linexp (0, 1, 0.001, 1)))
      & (tan)
      & (cos)
      & (+(W audio * Y uvN))
      & (fract)
      & (*(Y audio & linexp (0, 1, 1, 10)))
      & (tan)
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+vec2(0, -0.33))
          . mirror
          . rot (0.3 * W audio* m)
          . (^*(X audio & linexp (0, 1, 1.1, 0.9)))
          . (+vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)


candyRoad3 = 0.01 *^ vec4 (r v, g v , b v, 1) + 0.99 *^ bb
  -- & clamp 0 1
  where
    r :: Vec1 -> Vec1
    r v = Y audio * cos(X audio * 10 * sin(tim + Y uvN * 100)) ** 0.5

    g :: Vec1 -> Vec1
    g v = Y audio * cos(X audio* 10 * sin(tim + X uvN * 100)) ** 0.1

    b :: Vec1 -> Vec1
    b v = Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) **2

    v = 1

    tim = time * 0.01
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.2) * W audio* m)
          . mirror
          . rot ((-0.2) * W audio* m)
          . (^*(X audio & linexp (0, 1, 1.1, 0.85)))
          . (+vec2 (-k, k))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = X audio & linexp (0, 1, 0.000001, 0.001)

candyRoad4 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *Y audio * cos(X audio* 10 * sin(tim + X uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) **0.1

    tim = time * 0.01

    v = radius (uvN - vec2 (0.1, 0)) * Z audio
      & (*(X audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * (W audio+ X audio)* m)
          . (^*((X audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * W audio* m)
          . (+vec2 (-k, 0))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = Y audio & linexp (0, 1, 0.0000001, 0.001)

candyRoad5 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r v = v *Y audio * cos(X audio * 10 * sin(tim + 10 *Y uvN)) ** 1

    g v = v *Y audio * cos(X audio* 10 * sin(tim + 10 *Y uvN)) ** 1

    b v = v *Y audio * cos(X audio * 10 * sin(tim + 10 *Y uvN)) ** 1

    tim = time * 0.01

    v = radius (uvN - vec2 (0.1, 0)) * Z audio
      & (*(X audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * (W audio+ X audio)* m)
          . (^*((X audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * W audio* m)
          . (+vec2 (-k, 0))
          . (**0.999)
          . (sin)

    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = Y audio & linexp (0, 1, 0.0000001, 0.001)
-- TODO: make flash

candyRoad6 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *Y audio * cos(X audio* 10 * sin(tim + X uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) **0.1

    tim = time * 0.01

    v = radius (uvN - vec2 (0.1, 0)) * Z audio
      & (*(X audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . (+vec2 (0, negate disp))
          . rot ((-0.01) * (W audio)* m)
          . (^*((X audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * W audio)
          . (+vec2 (0, disp))
          . (+vec2 (-k, 0))
    disp = 0.25
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = Y audio & linexp (0, 1, 0.0000001, 0.001)

audioGraph1 :: Vec4
audioGraph1 = v
  where
    resolution = 100
    isDrawn = abs (X uvN  -  (1 / resolution)) `lt` (1/resolution)

    v = select isDrawn fresh old
    fresh = vec4 (v, v, v, 1)
      where
        v = Z audio * Y uv
          & (*10)
          & (tan)
        mul = 8

    old = texture2D backBuffer pos
      where
        pos = uvN
          & (\x -> vec2 (abs (X x), Y x))
          & (\x -> vec2 (X x, Y x * (X audio & linexp (0, 1, 0.8, 1.1))))
          & (\x -> x - vec2 (1/resolution * signum (X uvN), 0))
          & (rot (Y mouse * 0.1))
          & (\x -> x * 0.5 + 0.5)

audioGraph2 :: Vec4
audioGraph2 = v
  where
    resolution = 100
    isDrawn = abs (X uvN  -  (1 / resolution)) `lt` (1/resolution)

    v = select isDrawn fresh old
    fresh = vec4 (v, v, v, 1)
      where

        v = Z audio * Y uv
          & (*4)
          & fract
        mul = 8

    old = texture2D backBuffer pos
      where
        pos = uvN
          & (\x -> vec2 ((X x), (Y x)))
          & (\x -> vec2 (X x, Y x * (X audio & linexp (0, 1, 0.8, 1.1))))
          & (\x -> x - vec2 (1/resolution * signum (X uvN), 0))
          & (rot (X audio * 0.02))
          & (\x -> x * 0.5 + 0.5)

main = putStrLn . toGLSL $ audioGraph2
