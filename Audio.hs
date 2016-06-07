{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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

sigmoid :: Vec1 -> Vec1
sigmoid x = recip (1 + exp (negate x))

phi uv' = atan (y_ uv'/ x_ uv')





myColor1 :: Vec4
myColor1 = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + w_ audio
    g = v * y_ audio
    b = v * z_ audio

    v = cos (len uvN * x_ audio * 100 + time)
      + 0.9 *^ tan (x_ uvN * x_ audio * 10 + time)
    bb = texture2D backBuffer ((rot (z_ audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)



myColor2 :: Vec4
myColor2 = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + w_ audio
    g = v * y_ audio
    b = v * z_ audio

    v = cos (len uvN * x_ audio * 100 + time)
      + 0.9 *^ sin (x_ uvN * x_ audio * 10 + time)
    bb = texture2D backBuffer ((rot (z_ audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)





















myColor :: Vec4
myColor = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + w_ audio
    g = v * y_ audio
    b = v * z_ audio

    v = v' * z_ audio
    v' =recip $ tan  (y_ uvN * x_ audio * 10 + tim)
      + 0.9 *^ sin (x_ bb  * x_ audio * 10 +tim )
    bb = texture2D backBuffer ((rot (z_ audio) (((y_ audio *^ uvN)) * 0.9)) * 0.5 + 0.5)
    tim = time * 0.1


myColor3 = 0.1 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v' * w_ audio
    g = v' * z_ audio
    b = v' * y_ audio
    v' = product $ map v [0..10]
    v x = tan ((x_ audio + phi uvN') + time * 0.01 + w_ audio)
      where
        uvN' = rot time (y_ audio *^ vec2 (y_ uvN , x_ uvN) + (z_ audio *^ 0.2) * fromInteger (x))
    bb = texture2D backBuffer (0.5 * (rot tim (uvN * 1.1)) + 0.5)
    tim = time * 0.1














mirror :: Vec2 -> Vec2
mirror v = vec2 (abs $ x_ v, y_ v)

myColor4 = 0.01 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v * x_ uvN
    g = v * y_ uvN
    b = v
    v = len (uvN - mouse) * (10 * x_ audio)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror

myColor5 = 0.01 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v * x_ uvN
    g = v * y_ uvN
    b = v
    v = len (uvN - mouse) * (100 * x_ audio)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror


spaceKandinsky = 0.01 *^ vec4 (r,g ,b, 1) + 1.01 *^ bb
  where
    r = v * fract (x_ uvN * 100)
    g = v * fract (y_ uvN * 100)
    b = v
    v = len (uvN - mouse) * 10
      & \x -> 10 * sin x
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (pi/2) . (*(0.5)) . mirror

dashiki= 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    mul = 100
    r = v * fract (x_ uvN * mul)
    g = v * fract (y_ uvN * mul)
    b = v
    tim = time/ 10e1
    v = len (uvN - mouse + vec2 (sin tim, cos tim)) * y_ mouse
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
    fn n = y_ (uvN - mouse + vec2(sin tim, cos tim))
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
    g = v * y_ audio * x_ uvN
    b = v * z_ audio * fract (len (uvN * 100))
    r = v * w_ audio * cos (len (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = y_ (vec2 (x_ uvN, (-1) * y_ uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * w_ audio)
          . (^*(x_ audio & linexp (0, 1, 1, 0.9)))

cottonCandy2 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * y_ audio * x_ uvN
    r = 1 - v * z_ audio * fract (len (uvN * 100))
    g = 0.5 + v * w_ audio * cos (len (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = y_ (vec2 (x_ uvN, (-1) * y_ uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * w_ audio)
          . (^*(x_ audio & linexp (0, 1, 1, 0.9)))

cottonCandy3 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * y_ audio * x_ uvN
    r = v * z_ audio * fract (len (uvN * 100))
    g = v * w_ audio * cos (len (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = y_ (vec2 (x_ uvN, y_ uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (pi/ 6 * w_ audio)
          . (^*(x_ audio & linexp (0, 1, 1, 0.9)))

cottonCandy4 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    r = v * x_ uvN
    g = v * y_ uvN
    b = v
    v = tan $ len (uvN - mouse) * x_ audio * 100
    bb = texture2D backBuffer (0.5 * fn (uvN) + 0.5)
      where
        fn = id
          . mirror
          . rot (time * 0.001)
          . (^*(x_ audio & linexp (0, 1, 1, 0.9)))

-- main = putStrLn . toGLSL $ gameOfLife
-- gameOfLife = vec4 (v, v, v, 1)
--   where
--     v = sel false 1 0

candyRoad = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 1 -  v * y_ audio * x_ uvN
    r = v * z_ audio * fract (len (uvN * 100))
    g = v * w_ audio * cos (len (uvN * 100))
    v = product $ map fn [0..10]
    fn n = y_ (vec2 (x_ uvN, y_ uvN))
      & (\x -> 10 * x + (fromInteger n) * 10)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+vec2(0, -0.33))
          . mirror
          . rot (0.3* w_ audio* m)
          . (^*(x_ audio & linexp (0, 1, 1.1, 0.9)))
          . (+vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)

candyRoad2 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ bb
  where
    r :: Vec1 -> Vec1
    r v = v - tan (v * 0.001 + sin(tim + y_ uvN)) ** 0.1

    g :: Vec1 -> Vec1
    g v = v - tan (v * 0.001 + sin(tim + x_ uvN)) ** 0.3

    b :: Vec1 -> Vec1
    b v = v - tan (v * 0.001 + sin(tim + len uvN)) ** 0.4

    tim = time * 0.01

    v = (x_ audio & linexp (0, 1, 1, 100)) - len (uvN - vec2 (0.5 * y_ audio, -0.333)) * y_ audio
      & (*(x_ audio & linexp (0, 1, 0.001, 1)))
      & (tan)
      & (cos)
      & (+(w_ audio * y_ uvN))
      & (fract)
      & (*(y_ audio & linexp (0, 1, 1, 10)))
      & (tan)
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+vec2(0, -0.33))
          . mirror
          . rot (0.3 * w_ audio* m)
          . (^*(x_ audio & linexp (0, 1, 1.1, 0.9)))
          . (+vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)


candyRoad3 = 0.01 *^ vec4 (r v, g v , b v, 1) + 0.99 *^ bb
  -- & clamp 0 1
  where
    r :: Vec1 -> Vec1
    r v = y_ audio * cos(x_ audio * 10 * sin(tim + y_ uvN * 100)) ** 0.5

    g :: Vec1 -> Vec1
    g v = y_ audio * cos(x_ audio* 10 * sin(tim + x_ uvN * 100)) ** 0.1

    b :: Vec1 -> Vec1
    b v = y_ audio * cos(x_ audio * 10 * sin(tim + x_ uvN * 100)) **2

    v = 1

    tim = time * 0.01
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.2) * w_ audio* m)
          . mirror
          . rot ((-0.2) * w_ audio* m)
          . (^*(x_ audio & linexp (0, 1, 1.1, 0.85)))
          . (+vec2 (-k, k))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = x_ audio & linexp (0, 1, 0.000001, 0.001)

candyRoad4 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *y_ audio * cos(x_ audio * 10 * sin(tim + x_ uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *y_ audio * cos(x_ audio* 10 * sin(tim + x_ uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *y_ audio * cos(x_ audio * 10 * sin(tim + x_ uvN * 100)) **0.1

    tim = time * 0.01

    v = len (uvN - vec2 (0.1, 0)) * z_ audio
      & (*(x_ audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * (w_ audio+ x_ audio)* m)
          . (^*((x_ audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * w_ audio* m)
          . (+vec2 (-k, 0))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = y_ audio & linexp (0, 1, 0.0000001, 0.001)

candyRoad5 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r v = v *y_ audio * cos(x_ audio * 10 * sin(tim + 10 *y_ uvN)) ** 1

    g v = v *y_ audio * cos(x_ audio* 10 * sin(tim + 10 *y_ uvN)) ** 1

    b v = v *y_ audio * cos(x_ audio * 10 * sin(tim + 10 *y_ uvN)) ** 1

    tim = time * 0.01

    v = len (uvN - vec2 (0.1, 0)) * z_ audio
      & (*(x_ audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * (w_ audio+ x_ audio)* m)
          . (^*((x_ audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * w_ audio* m)
          . (+vec2 (-k, 0))
          . (**0.999)
          . (sin)

    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = y_ audio & linexp (0, 1, 0.0000001, 0.001)
-- TODO: make flash

candyRoad6 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *y_ audio * cos(x_ audio * 10 * sin(tim + x_ uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *y_ audio * cos(x_ audio* 10 * sin(tim + x_ uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *y_ audio * cos(x_ audio * 10 * sin(tim + x_ uvN * 100)) **0.1

    tim = time * 0.01

    v = len (uvN - vec2 (0.1, 0)) * z_ audio
      & (*(x_ audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . (+vec2 (0, negate disp))
          . rot ((-0.01) * (w_ audio)* m)
          . (^*((x_ audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * w_ audio)
          . (+vec2 (0, disp))
          . (+vec2 (-k, 0))
    disp = 0.25
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = y_ audio & linexp (0, 1, 0.0000001, 0.001)

audioGraph1 :: Vec4
audioGraph1 = v
  where
    resolution = 100
    isDrawn = abs (x_ uvN  -  (1 / resolution)) `lt` (1/resolution)

    v = sel isDrawn fresh old
    fresh = vec4 (v, v, v, 1)
      where
        v = z_ audio * y_ uv
          & (*10)
          & (tan)
        mul = 8

    old = texture2D backBuffer pos
      where
        pos = uvN
          & (\x -> vec2 (abs (x_ x), y_ x))
          & (\x -> vec2 (x_ x, y_ x * (x_ audio & linexp (0, 1, 0.8, 1.1))))
          & (\x -> x - vec2 (1/resolution * signum (x_ uvN), 0))
          & (rot (y_ mouse * 0.1))
          & (\x -> x * 0.5 + 0.5)

audioGraph2 :: Vec4
audioGraph2 = v
  where
    resolution = 100
    isDrawn = abs (x_ uvN  -  (1 / resolution)) `lt` (1/resolution)

    v = sel isDrawn fresh old
    fresh = vec4 (v, v, v, 1)
      where

        v = z_ audio * y_ uv
          & (*4)
          & fract
        mul = 8

    old = texture2D backBuffer pos
      where
        pos = uvN
          & (\x -> vec2 (abs(x_ x), abs (y_ x)))
          & (\x -> vec2 (x_ x, y_ x * (x_ audio & linexp (0, 1, 0.8, 1.1))))
          & (\x -> x - vec2 (1/resolution * signum (x_ uvN), 0))
          & (rot (x_ audio * (1 * 0.05)))
          & (\x -> x * 0.5 + 0.5)

gameOfLifeAudio :: Vec4
gameOfLifeAudio = vec4 (v, v, v, 1) * vec4 (c, c, c, 1)
  where
    c = 1
    downsample :: (Veccable a) => Vec a -> Vec a -> Vec a
    downsample a x = floor_ (a*x) / a

    res = copy $ floor_ $ x_ resolution /2

    duv = downsample res uvN

    shift = vec2 (0, 0);

    maxi= downsample res $ shift + size/res
    mini= downsample res $ shift + (negate $ size/res)

    size = vec2 (z_ audio - 0.5, w_ audio) * 500

    drawWithMouse = product [ duv `lt` maxi
                            , duv `geq` mini
                            ]

    v = (sel drawWithMouse 1 $ sel alive 1 0)
      where
        alive = sum [rule1, rule3]

        neighbors = [ (-1,-1), (-1, 0), (-1, 1)
                    , ( 0,-1),          ( 0, 1)
                    , ( 1,-1), ( 1, 0), ( 1, 1)
                    ]

        numAlive :: Vec1
        numAlive = sum $ map getVal neighbors
          where
            getVal offset = z_ $ texture2D backBuffer
              $ id             ( uvN
                                 & (\x -> (res * x + vec2 offset) / res)
                                 & (\x -> x * 0.5 + 0.5)
                               )

        wasAlive = val `gt` 0
          where
            val = x_ $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

        rule1 = wasAlive * ((numAlive `eq` 2) + (numAlive `eq` 3))
        rule3 = negate wasAlive * numAlive `eq` 3

rclogo1 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (texture2D channel1 (f uvN)) (vec4 (1, 1, 1, 1))
    f = id
      >>> (\x -> x - mouse)
      >>> (\v -> vec2 (x_ v, negate $ y_ v))
      >>> (^*(sin time + 2))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*1.5)

rclogo2 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (z_ audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      -- >>> (\x -> x - mouse)
      >>> (\v -> vec2 (x_ v, negate $ y_ v))
      >>> (^*(2 * exp (x_ audio) + exp (w_ audio)))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (x_ v + z_ audio + 0.1 * time, y_ v + w_ audio + 0.1 * time))
      >>> (fract)
      >>> (^*(4 * y_ audio))
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (rot (0.03 * y_ audio))
      -- >>> (\x -> x - mouse)
      >>> (\x -> x ^* (x_ audio + 0.1))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (abs $ x_ v, abs$ y_ v))
      >>> (fract)

rclogo3 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (z_ audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 (x_ v, negate $ y_ v))
      >>> (^*(1 * exp (x_ audio) + exp (w_ audio)))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (x_ v + speed * time, y_ v + 0.5))
      -- >>> (\v -> vec2 (x_ v + z_ audio + 0.1 * time, y_ v + w_ audio + 0.1 * time))
      >>> (fract)
      >>> (^*(4 * y_ audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  x_ v, abs$ y_ v))
      >>> (^/mul)
      >>> (rot (0.03 * y_ audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* (x_ audio + 0.1))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo4 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (z_ audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 (x_ v, negate $ y_ v))
      >>> (^*(tan $ x_ audio))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (x_ v + 0.25, y_ v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * y_ audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  x_ v, abs$ y_ v))
      >>> (^/mul)
      >>> (rot (0.05 * y_ audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* (x_ audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo5 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (z_ audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 (x_ v, negate $ y_ v))
      >>> (^*(tan $ exp $ x_ audio / 5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (x_ v + 0.25, y_ v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * y_ audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  x_ v, abs$ y_ v))
      >>> (^/mul)
      >>> (rot (0.05 * y_ audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* (z_ audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo6 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (z_ audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 (x_ v, negate $ y_ v))
      >>> (^*(tan $ exp $ x_ audio / 5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (x_ v + 0.25, y_ v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * y_ audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  x_ v, abs $  y_ v))
      >>> (^/mul)
      >>> (rot (0.05 * y_ audio))
      >>> (rot (cos$ len uvN))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* (z_ audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo7 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) ((\x -> vec4 (vec3 (z_ x, x_ x, y_ x), w_ x))bb)
    stuff = texture2D channel1 (f uvN)
    f = id
      >>> (\v -> vec2 (x_ v, negate $ y_ v))
      >>> (^*(x_ audio * 2 * pi/3))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (x_ v + 0.25, y_ v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * y_ audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  x_ v, abs $  y_ v))
      >>> (^/mul)
      >>> (rot (0.2 * y_ audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* (z_ audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo8 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (id bb)
    -- rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    stuff = texture2D channel1 (ff . f $ uvN)
    f = id
      >>> (\x -> x - mouse)
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (*10)

    ff = id
      >>> (\x -> x * 0.5 + 0.5)
      -- >>> (\x -> x + copy (time * 0.1))
      -- >>> (fract)
      -- >>> (*2)
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> x * (1 + 0.1))
      -- >>> (\x -> vec2 (x & x_ & abs, x & Y))
      -- >>> (rot (x_ audio * (1 * 0.1)))
      >>> (\x -> x * 0.5 + 0.5)
    mul = 20

rclogo9 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (fade bb)
    -- rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.9
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x_ x , x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + copy (time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x & x_  & negate & abs, x & y_ ))
      >>> (\x -> x * (1 + 0.1))
      >>> (rot (x_ audio * 0.2))
      >>> (\x -> x - mouse)
      >>> (\x -> x * 0.5 + 0.5)
    mul = 20

rclogo10 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (fade bb)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.9
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + copy (time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.25
    bb = texture2D backBuffer (g uvN)
    g = id
      -- >>> (\x -> vec2 (x & x_  & negate & abs, x & y_ ))
      >>> (\x -> x ^* (0.4 + x_ audio))
      -- >>> (rot (x_ audio * 0.2))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time

rclogo11 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x & x_  & abs, x & y_  & abs))
      >>> (\x -> x ^* (0.4 + x_ audio))
      >>> (rot (x_ audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time * 0.01

rclogo12 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN) * vec4 (v, v, v, 1)
      where
        v = y_ uvN
          & (*(w_ audio * 10))
    f = id
      >>> (\x -> vec2 (x_ x, y_ x & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x_ x  & abs, y_ x  & abs))
      >>> (\x -> x ^* (0.4 + x_ audio))
      >>> (rot (x_ audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time * 0.01

rclogo13 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN) * vec4 (v, v, v, 1)
      where
        v = y_ uvN
          & (*(3 -  3*z_ audio))
          & (cos)
          & (*(w_ audio * 10))
    f = id
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      -- >>> (\x -> vec2 (x & x_  & abs, x & y_  & abs))
      >>> (\x -> x ^* (0.4 + x_ audio))
      -- >>> (rot (x_ audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time * 2

rclogo14 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v (w_ audio * 10)
    v2 = v (y_ audio)
      & clamp 0.5 1

    v x= len uvN
      & (*(3 -  3*y_ audio))
      & (cos)
      & (*(x))

    f = id
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      * vec4 (v2, v2, v2, 1)
    g = id
      -- >>> (\x -> vec2 (x & x_  & abs, x & y_  & abs))
      >>> (\x -> x ^* (0.4  + x_ audio))
      -- >>> (rot (x_ audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo15 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (rotColor . fade$  bb)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (ff . f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v (w_ audio * 10)
    v2 = v (y_ audio)

    v x= len uvN
      & (*(3 -  3*y_ audio))
      & (cos)
      & (*(x))
      & clamp 0.5 1

    f = id
      >>> (\x -> x - mouse)
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
    ff = id
      >>> (\x -> x * 0.5 + 0.5)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      -- * vec4 (v2, v2, v2, 1)
    g = id
      >>> (\x -> vec2 (x & x_  & abs, x & y_  & id))
      >>> (\x -> x ^* (0.4  + x_ audio))
      >>> (rot (x_ audio * 0.4))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo16 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel ((ff . f) uvN `lt` 1 * (ff . f) uvN `gt` (-1))
      (rotColor $ mix (w_ stuff) stuff bb) (rotColor . fade$  bb)
    rotColor x = vec4 (vec3 (z_ x * a, x_ x *b, y_ x *c), w_ x)
    a = sin (time * 0.1 + 1) * 0.5 + 0.8
    b = sin (time * 0.1) * 0.5 + 0.8
    c = 1

    fade x = x ** 0.9
    stuff = texture2D channel1 (ff . f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v (w_ audio * 10)
    v2 = v (y_ audio)

    v x= len uvN
      & (*(3 -  3*y_ audio))
      & (cos)
      & (*(x))
      & clamp 0.5 1

    f = id
      >>> (\x -> x - mouse)
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
    ff = id
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      -- * vec4 (v2, v2, v2, 1)
    g = id
      -- >>> (\x -> vec2 (x & x_  & abs, x & y_  & id))
      >>> (\x -> x ^* (0.4  + x_ audio))
      -- >>> (rot (x_ audio * 0.4))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo17 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (fade $ mix (w_ stuff) stuff bb) (rotColor . fade$  bb)
    rotColor x = vec4 (vec3 (1 - y_ x, z_ x, 1 -  x_ x), w_ x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v (w_ audio * 10)
    v2 = v (y_ audio)
      & clamp 0.5 1

    v x= len uvN
      & (*(3 -  3*y_ audio))
      & (cos)
      & (*(x))

    f = id
      >>> (\x -> vec2 (x_ x, y_ x & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      * vec4 (v2, v2, v2, 1)
    g = id
      >>> (\x -> vec2 (x & x_  & abs, x & y_  & abs))
      >>> (\x -> x ^* (0.4  + x_ audio))
      >>> (rot (x_ audio * 0.2))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo18 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = sel (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix (w_ stuff) stuff bb) (fade bb)
    rotColor x = vec4 (vec3 (z_ x, x_ x, y_ x), w_ x)
    fade x = x ** 0.90
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x_ x, x & y_ & negate))
      >>> (^*(y_ audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + copy (time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x & x_  & negate & abs, x & y_ ))
      >>> (\x -> x ^* (0.4 + x_ audio))
      >>> (rot (x_ audio * 0.2))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time

rclogo19 = logosAndTrip
  where
    whiteify v = mix (w_ v) (vec4 (x_ v, y_ v, z_ v, 1)) (vec4 (1, 1, 1, 1))
    colorTrans v = vec4 (1 - y_ v, 1 - z_ v, 1 - x_ v, w_ v)
    rotColor v = vec4 (y_ v, z_ v, x_ v, w_ v)
    logosAndTrip = whiteify $ mix (w_ logos) logos (bb)
    fade x = x ** 0.90
      where
        k = y_ mouse
    bb =  fade . rotColor $ texture2D backBuffer (g uvN)
      where
        g = id
          >>> (\x -> x * copy m)
          -- >>> (\x -> vec2 (abs $ x_ x, abs $ y_ x))
          -- >>> (rot (w_ audio * y_ mouse))
          >>> (\x -> x - copy (sin (time * 0.1) * 0.02))
          >>> (\x -> x * 0.5 + 0.5)
        m = 0.5 + x_ audio
    logos = foldl (\x y -> mix (w_ y) (y) (x)) 0 $ [ logo (-1) 
                                                  , logo 0
                                                  , logo 1
                                                  ]
    logo e = withOpacity foo $ chan1OrWhite (vec2 (0, n) + vec2(fract (time * 0.1 + n * 0.5) * 2 - 1, 0))
      where
        -- foo = 1
        foo = sin (time + n * 4 * pi/3) * 0.33 + 0.66
        n = fromInteger e
        withOpacity k v = vec4 (x_ v, y_ v, z_ v, k * w_ v)
        chan1OrWhite input  = sel ((ff . f) uvN `lt` 1 * (ff . f) uvN `gt` (-1))
          (texture2D channel1 (ff . f $  uvN)) (vec4 (1, 1, 1, 0))
          where
            f = id
              >>> (\v -> v + input)
              >>> (\v -> vec2 (x_ v, negate $ y_ v))
              >>> (rot t)
              >>> (\x -> x ^* (x_ audio * (5 + n * 2)))
            ff = id
              >>> (\x -> x * 0.5 + 0.5)
              >>> (fract)
              >>> (\x -> x * 5)
            t = sin (time * 0.5) * 0.5

over :: Vec4 -> Vec4 -> Vec4
over x y = mix (a) x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - w_ x'

fresh = color `over` bb where
  bb = texture2D backBuffer (f uvN)
  color = sel (abs uvN `lt` 0.5) (vec4(1, 1, 1, 1)) (vec4(0, 0, 0, 1))
  f = id
    >>> (\x -> x*0.9)
    >>> (\x -> x*0.5 + 0.5)




output = toProgram fresh



