{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Audio where

import Hylogen.WithHylide
import Data.VectorSpace
import Data.Function
import Control.Arrow

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * ((!X) a)
                   + sin phi * ((!Y) a)
                 , (-1) * sin phi * ((!X) a)
                   + cos phi * ((!Y) a)
                 )

sigmoid :: Vec1 -> Vec1
sigmoid x = recip (1 + exp (negate x))

phi uv' = atan ((!Y) uv'/ (!X) uv')





myColor1 = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + (!W) audio
    g = v * (!Y) audio
    b = v * (!Z) audio

    v = cos (len uvN * (!X) audio * 100 + time)
      + 0.9 *^ tan ((!X) uvN * (!X) audio * 10 + time)
    bb = texture2D backBuffer ((rot ((!Z) audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)



myColor2 = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + (!W) audio
    g = v * (!Y) audio
    b = v * (!Z) audio

    v = cos (len uvN * (!X) audio * 100 + time)
      + 0.9 *^ sin ((!X) uvN * (!X) audio * 10 + time)
    bb = texture2D backBuffer ((rot ((!Z) audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)






















myColor = (0.1 *^ vec4 (r, g, b, 1) + bb)
  where
    r = v + (!W) audio
    g = v * (!Y) audio
    b = v * (!Z) audio

    v = v' * (!Z) audio
    v' =recip $ tan  ((!Y) uvN * (!X) audio * 10 + tim)
      + 0.9 *^ sin ((!X) bb  * (!X) audio * 10 +tim )
    bb = texture2D backBuffer ((rot ((!Z) audio) ((((!Y) audio *^ uvN)) * 0.9)) * 0.5 + 0.5)
    tim = time * 0.1


myColor3 = 0.1 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v' * (!W) audio
    g = v' * (!Z) audio
    b = v' * (!Y) audio
    v' = product $ map v [0..10]
    v x = tan (((!X) audio + phi uvN') + time * 0.01 + (!W) audio)
      where
        uvN' = rot time ((!Y) audio *^ vec2 ((!Y) uvN , (!X) uvN) + ((!Z) audio *^ 0.2) * fromInteger (x))
    bb = texture2D backBuffer (0.5 * (rot tim (uvN * 1.1)) + 0.5)
    tim = time * 0.1














mirror :: Vec2 -> Vec2
mirror v = vec2 (abs $ (!X) v, (!Y) v)

myColor4 = 0.01 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v * (!X) uvN
    g = v * (!Y) uvN
    b = v
    v = len (uvN - mouse) * (10 * (!X) audio)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror

myColor5 = 0.01 *^ vec4 (r,g ,b, 1) + bb
  where
    r = v * (!X) uvN
    g = v * (!Y) uvN
    b = v
    v = len (uvN - mouse) * (100 * (!X) audio)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror


spaceKandinsky = 0.01 *^ vec4 (r,g ,b, 1) + 1.01 *^ bb
  where
    r = v * fract ((!X) uvN * 100)
    g = v * fract ((!Y) uvN * 100)
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
    r = v * fract ((!X) uvN * mul)
    g = v * fract ((!Y) uvN * mul)
    b = v
    tim = time/ 10e1
    v = len (uvN - mouse + vec2 (sin tim, cos tim)) * (!Y) mouse
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
    fn n = (!Y) (uvN - mouse + vec2(sin tim, cos tim))
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
    g = v * (!Y) audio * (!X) uvN
    b = v * (!Z) audio * fract (len (uvN * 100))
    r = v * (!W) audio * cos (len (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = (!Y) (vec2 ((!X) uvN, (-1) * (!Y) uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * (!W) audio)
          . (^*((!X) audio & linexp (0, 1, 1, 0.9)))

cottonCandy2 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * (!Y) audio * (!X) uvN
    r = 1 - v * (!Z) audio * fract (len (uvN * 100))
    g = 0.5 + v * (!W) audio * cos (len (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = (!Y) (vec2 ((!X) uvN, (-1) * (!Y) uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * (!W) audio)
          . (^*((!X) audio & linexp (0, 1, 1, 0.9)))

cottonCandy3 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * (!Y) audio * (!X) uvN
    r = v * (!Z) audio * fract (len (uvN * 100))
    g = v * (!W) audio * cos (len (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = (!Y) (vec2 ((!X) uvN, (!Y) uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (pi/ 6 * (!W) audio)
          . (^*((!X) audio & linexp (0, 1, 1, 0.9)))

cottonCandy4 = 0.01 *^ vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    r = v * (!X) uvN
    g = v * (!Y) uvN
    b = v
    v = tan $ len (uvN - mouse) * (!X) audio * 100
    bb = texture2D backBuffer (0.5 * fn (uvN) + 0.5)
      where
        fn = id
          . mirror
          . rot (time * 0.001)
          . (^*((!X) audio & linexp (0, 1, 1, 0.9)))

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
    b = 1 -  v * (!Y) audio * (!X) uvN
    r = v * (!Z) audio * fract (len (uvN * 100))
    g = v * (!W) audio * cos (len (uvN * 100))
    v = product $ map fn [0..10]
    fn n = (!Y) (vec2 ((!X) uvN, (!Y) uvN))
      & (\x -> 10 * x + (fromInteger n) * 10)
      & tan
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+vec2(0, -0.33))
          . mirror
          . rot (0.3* (!W) audio* m)
          . (^*((!X) audio & linexp (0, 1, 1.1, 0.9)))
          . (+vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)

candyRoad2 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ bb
  where
    r :: Vec1 -> Vec1
    r v = v - tan (v * 0.001 + sin(tim + (!Y) uvN)) ** 0.1

    g :: Vec1 -> Vec1
    g v = v - tan (v * 0.001 + sin(tim + (!X) uvN)) ** 0.3

    b :: Vec1 -> Vec1
    b v = v - tan (v * 0.001 + sin(tim + len uvN)) ** 0.4

    tim = time * 0.01

    v = ((!X) audio & linexp (0, 1, 1, 100)) - len (uvN - vec2 (0.5 * (!Y) audio, -0.333)) * (!Y) audio
      & (*((!X) audio & linexp (0, 1, 0.001, 1)))
      & (tan)
      & (cos)
      & (+((!W) audio * (!Y) uvN))
      & (fract)
      & (*((!Y) audio & linexp (0, 1, 1, 10)))
      & (tan)
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+vec2(0, -0.33))
          . mirror
          . rot (0.3 * (!W) audio* m)
          . (^*((!X) audio & linexp (0, 1, 1.1, 0.9)))
          . (+vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)


candyRoad3 = 0.01 *^ vec4 (r v, g v , b v, 1) + 0.99 *^ bb
  -- & clamp 0 1
  where
    r :: Vec1 -> Vec1
    r v = (!Y) audio * cos((!X) audio * 10 * sin(tim + (!Y) uvN * 100)) ** 0.5

    g :: Vec1 -> Vec1
    g v = (!Y) audio * cos((!X) audio* 10 * sin(tim + (!X) uvN * 100)) ** 0.1

    b :: Vec1 -> Vec1
    b v = (!Y) audio * cos((!X) audio * 10 * sin(tim + (!X) uvN * 100)) **2

    v = 1

    tim = time * 0.01
    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.2) * (!W) audio* m)
          . mirror
          . rot ((-0.2) * (!W) audio* m)
          . (^*((!X) audio & linexp (0, 1, 1.1, 0.85)))
          . (+vec2 (-k, k))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = (!X) audio & linexp (0, 1, 0.000001, 0.001)

candyRoad4 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *(!Y) audio * cos((!X) audio * 10 * sin(tim + (!X) uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *(!Y) audio * cos((!X) audio* 10 * sin(tim + (!X) uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *(!Y) audio * cos((!X) audio * 10 * sin(tim + (!X) uvN * 100)) **0.1

    tim = time * 0.01

    v = len (uvN - vec2 (0.1, 0)) * (!Z) audio
      & (*((!X) audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * ((!W) audio+ (!X) audio)* m)
          . (^*(((!X) audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * (!W) audio* m)
          . (+vec2 (-k, 0))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = (!Y) audio & linexp (0, 1, 0.0000001, 0.001)

candyRoad5 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r v = v *(!Y) audio * cos((!X) audio * 10 * sin(tim + 10 *(!Y) uvN)) ** 1

    g v = v *(!Y) audio * cos((!X) audio* 10 * sin(tim + 10 *(!Y) uvN)) ** 1

    b v = v *(!Y) audio * cos((!X) audio * 10 * sin(tim + 10 *(!Y) uvN)) ** 1

    tim = time * 0.01

    v = len (uvN - vec2 (0.1, 0)) * (!Z) audio
      & (*((!X) audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * ((!W) audio+ (!X) audio)* m)
          . (^*(((!X) audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * (!W) audio* m)
          . (+vec2 (-k, 0))
          . (**0.999)
          . (sin)

    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = (!Y) audio & linexp (0, 1, 0.0000001, 0.001)
-- TODO: make flash

candyRoad6 = 0.01 *^ vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *(!Y) audio * cos((!X) audio * 10 * sin(tim + (!X) uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *(!Y) audio * cos((!X) audio* 10 * sin(tim + (!X) uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *(!Y) audio * cos((!X) audio * 10 * sin(tim + (!X) uvN * 100)) **0.1

    tim = time * 0.01

    v = len (uvN - vec2 (0.1, 0)) * (!Z) audio
      & (*((!X) audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . (+vec2 (0, negate disp))
          . rot ((-0.01) * ((!W) audio)* m)
          . (^*(((!X) audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * (!W) audio)
          . (+vec2 (0, disp))
          . (+vec2 (-k, 0))
    disp = 0.25
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = (!Y) audio & linexp (0, 1, 0.0000001, 0.001)

audioGraph1 :: Vec4
audioGraph1 = v
  where
    resolution = 100
    isDrawn = abs ((!X) uvN  -  (1 / resolution)) `lt` (1/resolution)

    v = select isDrawn fresh old
    fresh = vec4 (v, v, v, 1)
      where
        v = (!Z) audio * (!Y) uv
          & (*10)
          & (tan)
        mul = 8

    old = texture2D backBuffer pos
      where
        pos = uvN
          & (\x -> vec2 (abs ((!X) x), (!Y) x))
          & (\x -> vec2 ((!X) x, (!Y) x * ((!X) audio & linexp (0, 1, 0.8, 1.1))))
          & (\x -> x - vec2 (1/resolution * signum ((!X) uvN), 0))
          & (rot ((!Y) mouse * 0.1))
          & (\x -> x * 0.5 + 0.5)

audioGraph2 :: Vec4
audioGraph2 = v
  where
    resolution = 100
    isDrawn = abs ((!X) uvN  -  (1 / resolution)) `lt` (1/resolution)

    v = select isDrawn fresh old
    fresh = vec4 (v, v, v, 1)
      where

        v = (!Z) audio * (!Y) uv
          & (*4)
          & fract
        mul = 8

    old = texture2D backBuffer pos
      where
        pos = uvN
          & (\x -> vec2 (abs ((!X) x), ((!Y) x)))
          & (\x -> vec2 ((!X) x, (!Y) x * ((!X) audio & linexp (0, 1, 0.8, 1.1))))
          & (\x -> x - vec2 (1/resolution * signum ((!X) uvN), 0))
          & (rot ((!X) audio * (1 * 0.05)))
          & (\x -> x * 0.5 + 0.5)

gameOfLifeAudio :: Vec4
gameOfLifeAudio = vec4 (v, v, v, 1) * vec4 (c, c, c, 1)
  where
    c = 1
    downsample :: (Veccable a) => Vec a -> Vec a -> Vec a
    downsample a x = floor_ (a*x) / a

    res = copy $ floor_ $ (!X) resolution /2

    duv = downsample res uvN

    shift = vec2 (0, 0);

    maxi= downsample res $ shift + size/res
    mini= downsample res $ shift + (negate $ size/res)

    size = vec2 ((!Z) audio - 0.5, (!W) audio) * 500

    drawWithMouse = product [ duv `lt` maxi
                            , duv `geq` mini
                            ]

    v = (select drawWithMouse 1 $ select alive 1 0)
      where
        alive = sum [rule1, rule3]

        neighbors = [ (-1,-1), (-1, 0), (-1, 1)
                    , ( 0,-1),          ( 0, 1)
                    , ( 1,-1), ( 1, 0), ( 1, 1)
                    ]

        numAlive :: Vec1
        numAlive = sum $ map getVal neighbors
          where
            getVal offset = (!Z) $ texture2D backBuffer
              $ id             ( uvN
                                 & (\x -> (res * x + vec2 offset) / res)
                                 & (\x -> x * 0.5 + 0.5)
                               )

        wasAlive = val `gt` 0
          where
            val = (!X) $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

        rule1 = wasAlive * ((numAlive `eq` 2) + (numAlive `eq` 3))
        rule3 = negate wasAlive * numAlive `eq` 3

rclogo1 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (texture2D channel1 (f uvN)) (vec4 (1, 1, 1, 1))
    f = id
      >>> (\x -> x - mouse)
      >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
      >>> (^*(sin time + 2))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*1.5)

rclogo2 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!Z) audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      -- >>> (\x -> x - mouse)
      >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
      >>> (^*(2 * exp ((!X) audio) + exp ((!W) audio)))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 ((!X) v + (!Z) audio + 0.1 * time, (!Y) v + (!W) audio + 0.1 * time))
      >>> (fract)
      >>> (^*(4 * (!Y) audio))
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (rot (0.03 * (!Y) audio))
      -- >>> (\x -> x - mouse)
      >>> (\x -> x ^* ((!X) audio + 0.1))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 (abs $ (!X) v, abs$ (!Y) v))
      >>> (fract)

rclogo3 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!Z) audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
      >>> (^*(1 * exp ((!X) audio) + exp ((!W) audio)))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 ((!X) v + speed * time, (!Y) v + 0.5))
      -- >>> (\v -> vec2 ((!X) v + (!Z) audio + 0.1 * time, (!Y) v + (!W) audio + 0.1 * time))
      >>> (fract)
      >>> (^*(4 * (!Y) audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  (!X) v, abs$ (!Y) v))
      >>> (^/mul)
      >>> (rot (0.03 * (!Y) audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* ((!X) audio + 0.1))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo4 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!Z) audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
      >>> (^*(tan $ (!X) audio))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 ((!X) v + 0.25, (!Y) v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * (!Y) audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  (!X) v, abs$ (!Y) v))
      >>> (^/mul)
      >>> (rot (0.05 * (!Y) audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* ((!X) audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo5 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!Z) audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
      >>> (^*(tan $ exp $ (!X) audio / 5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 ((!X) v + 0.25, (!Y) v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * (!Y) audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  (!X) v, abs$ (!Y) v))
      >>> (^/mul)
      >>> (rot (0.05 * (!Y) audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* ((!Z) audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo6 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!Z) audio + 0.5) (texture2D channel1 (f uvN)) bb) bb
    f = id
      >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
      >>> (^*(tan $ exp $ (!X) audio / 5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 ((!X) v + 0.25, (!Y) v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * (!Y) audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  (!X) v, abs $  (!Y) v))
      >>> (^/mul)
      >>> (rot (0.05 * (!Y) audio))
      >>> (rot (cos$ len uvN))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* ((!Z) audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo7 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) ((\x -> vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x))bb)
    stuff = texture2D channel1 (f uvN)
    f = id
      >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
      >>> (^*((!X) audio * 2 * pi/3))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (\v -> vec2 ((!X) v + 0.25, (!Y) v + speed * time))
      >>> (fract)
      >>> (^*(2))
      -- >>> (^*(3 * (!Y) audio))
    speed = 0.1
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (^*mul)
      >>> (\v -> vec2 (abs $  (!X) v, abs $  (!Y) v))
      >>> (^/mul)
      >>> (rot (0.2 * (!Y) audio))
      >>> (\x -> x - mouse)
      >>> (\x -> x ^* ((!Z) audio + 0.5))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
    mul = 20

rclogo8 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (id bb)
    -- rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    stuff = texture2D channel1 (ff . f $ uvN)
    f = id
      >>> (\x -> x - mouse)
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
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
      -- >>> (\x -> vec2 (x & (!X) & abs, x & Y))
      -- >>> (rot ((!X) audio * (1 * 0.1)))
      >>> (\x -> x * 0.5 + 0.5)
    mul = 20

rclogo9 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (fade bb)
    -- rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.9
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + copy (time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x & (!X)  & negate & abs, x & (!Y) ))
      >>> (\x -> x * (1 + 0.1))
      >>> (rot ((!X) audio * 0.2))
      >>> (\x -> x - mouse)
      >>> (\x -> x * 0.5 + 0.5)
    mul = 20

rclogo10 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (fade bb)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.9
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + copy (time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.25
    bb = texture2D backBuffer (g uvN)
    g = id
      -- >>> (\x -> vec2 (x & (!X)  & negate & abs, x & (!Y) ))
      >>> (\x -> x ^* (0.4 + (!X) audio))
      -- >>> (rot ((!X) audio * 0.2))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time

rclogo11 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x & (!X)  & abs, x & (!Y)  & abs))
      >>> (\x -> x ^* (0.4 + (!X) audio))
      >>> (rot ((!X) audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time * 0.01

rclogo12 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN) * vec4 (v, v, v, 1)
      where
        v = (!Y) uvN
          & (*((!W) audio * 10))
    f = id
      >>> (\x -> vec2 (x ! X, x ! Y & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x ! X  & abs, x ! Y  & abs))
      >>> (\x -> x ^* (0.4 + (!X) audio))
      >>> (rot ((!X) audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time * 0.01

rclogo13 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN) * vec4 (v, v, v, 1)
      where
        v = (!Y) uvN
          & (*(3 -  3*(!Z) audio))
          & (cos)
          & (*((!W) audio * 10))
    f = id
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      -- >>> (\x -> vec2 (x & (!X)  & abs, x & (!Y)  & abs))
      >>> (\x -> x ^* (0.4 + (!X) audio))
      -- >>> (rot ((!X) audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time * 2

rclogo14 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (fade$  bb)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v ((!W) audio * 10)
    v2 = v ((!Y) audio)
      & clamp 0.5 1

    v x= len uvN
      & (*(3 -  3*(!Y) audio))
      & (cos)
      & (*(x))

    f = id
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      * vec4 (v2, v2, v2, 1)
    g = id
      -- >>> (\x -> vec2 (x & (!X)  & abs, x & (!Y)  & abs))
      >>> (\x -> x ^* (0.4  + (!X) audio))
      -- >>> (rot ((!X) audio * 0.2 + pi))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo15 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (rotColor . fade$  bb)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (ff . f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v ((!W) audio * 10)
    v2 = v ((!Y) audio)

    v x= len uvN
      & (*(3 -  3*(!Y) audio))
      & (cos)
      & (*(x))
      & clamp 0.5 1

    f = id
      >>> (\x -> x - mouse)
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
    ff = id
      >>> (\x -> x * 0.5 + 0.5)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      -- * vec4 (v2, v2, v2, 1)
    g = id
      >>> (\x -> vec2 (x & (!X)  & abs, x & (!Y)  & id))
      >>> (\x -> x ^* (0.4  + (!X) audio))
      >>> (rot ((!X) audio * 0.4))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo16 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select ((ff . f) uvN `lt` 1 * (ff . f) uvN `gt` (-1))
      (rotColor $ mix ((!W) stuff) stuff bb) (rotColor . fade$  bb)
    rotColor x = vec4 (vec3 ((!Z) x * a, (!X) x *b, (!Y) x *c), (!W) x)
    a = sin (time * 0.1 + 1) * 0.5 + 0.8
    b = sin (time * 0.1) * 0.5 + 0.8
    c = 1

    fade x = x ** 0.9
    stuff = texture2D channel1 (ff . f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v ((!W) audio * 10)
    v2 = v ((!Y) audio)

    v x= len uvN
      & (*(3 -  3*(!Y) audio))
      & (cos)
      & (*(x))
      & clamp 0.5 1

    f = id
      >>> (\x -> x - mouse)
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
    ff = id
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      -- * vec4 (v2, v2, v2, 1)
    g = id
      -- >>> (\x -> vec2 (x & (!X)  & abs, x & (!Y)  & id))
      >>> (\x -> x ^* (0.4  + (!X) audio))
      -- >>> (rot ((!X) audio * 0.4))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo17 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (fade $ mix ((!W) stuff) stuff bb) (rotColor . fade$  bb)
    rotColor x = vec4 (vec3 (1 - (!Y) x, (!Z) x, 1 -  (!X) x), (!W) x)
    fade x = x ** 0.8
    stuff = texture2D channel1 (f $ uvN)
      * vec4 (v1, v1, v1, 1)

    v1 = v ((!W) audio * 10)
    v2 = v ((!Y) audio)
      & clamp 0.5 1

    v x= len uvN
      & (*(3 -  3*(!Y) audio))
      & (cos)
      & (*(x))

    f = id
      >>> (\x -> vec2 (x ! X, x ! Y & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + vec2 (0 + 0.5, time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
      * vec4 (v2, v2, v2, 1)
    g = id
      >>> (\x -> vec2 (x & (!X)  & abs, x & (!Y)  & abs))
      >>> (\x -> x ^* (0.4  + (!X) audio))
      >>> (rot ((!X) audio * 0.2))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r + r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.03
    p = time * 2

rclogo18 = logo 
  where
    logo = chan1OrWhite
    chan1OrWhite = select (f uvN `lt` 1 * f uvN `gt` (-1))
      (mix ((!W) stuff) stuff bb) (fade bb)
    rotColor x = vec4 (vec3 ((!Z) x, (!X) x, (!Y) x), (!W) x)
    fade x = x ** 0.90
    stuff = texture2D channel1 (f $ uvN)
    f = id
      >>> (\x -> vec2 (x ! X, x & (!Y) & negate))
      >>> (^*((!Y) audio & linexp (-1, 1, 1, 5)))
      >>> (\x -> x + copy (time * speed))
      >>> (\x -> x * 0.5 + 0.5)
      >>> (fract)
      >>> (*2)
    speed = 0.5
    bb = texture2D backBuffer (g uvN)
    g = id
      >>> (\x -> vec2 (x & (!X)  & negate & abs, x & (!Y) ))
      >>> (\x -> x ^* (0.4 + (!X) audio))
      >>> (rot ((!X) audio * 0.2))
      >>> (\x -> x - vec2 (cos p, sin p) ^* r)
      >>> (\x -> x * 0.5 + 0.5)
      >>> fract
    mul = 20
    r = 0.05
    p = time

rclogo19 = logosAndTrip
  where
    whiteify v = mix ((!W) v) (vec4 ((!X) v, (!Y) v, (!Z) v, 1)) (vec4 (1, 1, 1, 1))
    colorTrans v = vec4 (1 - (!Y) v, 1 - (!Z) v, 1 - (!X) v, (!W) v)
    rotColor v = vec4 ((!Y) v, (!Z) v, (!X) v, (!W) v)
    logosAndTrip = whiteify $ mix ((!W) logos) logos (bb)
    fade x = x ** 0.90
      where
        k = (!Y) mouse
    bb =  fade . rotColor $ texture2D backBuffer (g uvN)
      where
        g = id
          >>> (\x -> x * copy m)
          -- >>> (\x -> vec2 (abs $ (!X) x, abs $ (!Y) x))
          -- >>> (rot ((!W) audio * (!Y) mouse))
          >>> (\x -> x - copy (sin (time * 0.1) * 0.02))
          >>> (\x -> x * 0.5 + 0.5)
        m = 0.5 + (!X) audio
    logos = foldl (\x y -> mix ((!W) y) (y) (x)) 0 $ [ logo (-1) 
                                                  , logo 0
                                                  , logo 1
                                                  ]
    logo e = withOpacity foo $ chan1OrWhite (vec2 (0, n) + vec2(fract (time * 0.1 + n * 0.5) * 2 - 1, 0))
      where
        -- foo = 1
        foo = sin (time + n * 4 * pi/3) * 0.33 + 0.66
        n = fromInteger e
        withOpacity k v = vec4 ((!X) v, (!Y) v, (!Z) v, k * (!W) v)
        chan1OrWhite input  = select ((ff . f) uvN `lt` 1 * (ff . f) uvN `gt` (-1))
          (texture2D channel1 (ff . f $  uvN)) (vec4 (1, 1, 1, 0))
          where
            f = id
              >>> (\v -> v + input)
              >>> (\v -> vec2 ((!X) v, negate $ (!Y) v))
              >>> (rot t)
              >>> (\x -> x ^* ((!X) audio * (5 + n * 2)))
            ff = id
              >>> (\x -> x * 0.5 + 0.5)
              >>> (fract)
              >>> (\x -> x * 5)
            t = sin (time * 0.5) * 0.5



main = putStrLn . toGLSL $ gameOfLifeAudio



