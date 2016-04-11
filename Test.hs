{-# LANGUAGE NoMonomorphismRestriction #-}
module Test where

import Hylogen
import Data.VectorSpace
import Data.Function

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = Vec2 ( cos phi * (X a)
                   + sin phi * (Y a)
                 , (-1) * sin phi * (X a)
                   + cos phi * (Y a)
                 )

radius :: Vec2 -> Vec1
radius uv' = sqrt (X uv' ** 2 + Y uv' ** 2)

sigmoid :: Vec1 -> Vec1
sigmoid x = recip (1 + exp (negate x))

phi uv' = atan (Y uv'/ X uv')

world :: Vec4
world = Vec4 (r, g, b, 1)
  where
    gap = 10
    m = sin(time * 0.1) & linexp (-1, 1, 10e1, 10e10)
    ratemul = 0.5
    val   = cos(radius uvN * m + time + sin(time + gap) * ratemul)
    val'  = cos(radius uvN * m + time + sin(time + gap ** 2) * ratemul)
    val'' = cos(radius uvN * m + time + sin(time + gap ** 3) * ratemul)
    r = val ** 2
    g = val' ** 2
    b = val'' ** 2


poop  = Vec4 (r, g, b, 1)
  where
    tim = time * 0.1
      & \x -> tan x - sin x
    pos = Vec2 (X uv - tim * 2, Y uv - tim **2)
    val = sin $ radius pos * 10 * (sin $ (X pos * Y pos) / X audio)
    r = val ** 10
      + (X uv * Y uv + sin (time ** 0.2 * W audio * 2))
    g = (1 * val **0.5)
      - (cos (X uv * Y uv) + cos (time ** (0.5 * Z audio) + X audio))
      * (sin (X uv * 10 + time))
    b = (1 * val ** 2)
      - (X uv * Y uv + sin (time ** 0.1 + X audio))  - 1
      * (sin (X uv * 10 + time))


illusion =  Vec4 (x, x, x, 1)
  where
    x = sin (time
             & (*0.1)
             & \x -> uvN
                   & rot (sin $time)
                   & \uv' -> sin (sqrt(x))*10 / X uv' - (sin(sqrt x)) * 10 /Y uv')
      & \x -> 1/ sqrt x



coolio = Vec4 (v, v, v, 1)
  where
    circle uv' = 1 - radius uv'' * 10
      where
        uv'' = cos (uv' * 10)
    m = Vec2 (X mouse, Y mouse)

    circles = product $ map fn [0..10]
      where
        fn x = circle (uvN * m * fromInteger x)
    v = circles

cooooool =  Vec4 (v, v, v, 1)
  where
    circle uv' = tan $ 1 - radius uv'' * 10
      where
        uv'' = cos (uv' * 10)
    m = Vec2 (X mouse, Y mouse)

    circles = product $ map fn [0..11]
      where
        fn x = circle (uvN * m * fromVec1 (fromInteger x + 0.001*time))
    v = circles


comp = (0.01 * Vec4 (v, v, v, 1)  + bb)
  where
    v = product $ circle . fromInteger <$>  [0.. 2]
    circle x = (10 - 10 * radius (sin (uvN * 10 + 0.2 *  x) - m))
      & \x -> x / 4


    bb = Texture2D backBuffer ((uvN * 0.95) * 0.5 + 0.5)
    m = Vec2 (X mouse, Y mouse)


thang = (0.01 * Vec4 (v, v, v, 1)  + bb)
  where
    v = product $ circle . fromInteger <$>  [0.. 4]
    circle x = (10 - 10 * radius (sin (uvN * 10 + 0.2 *  x ) + 0.5 * Vec2 (cos $ time * 10, sin $  time * 10)))
      & \x -> x / 5


    bb = Texture2D backBuffer ((uvN * (Vec2(X audio, X audio))) * 0.5 + 0.5)
    m = Vec2 (X mouse, Y mouse)

myColor1 = (0.1 *^ Vec4 (r, g, b, 1) + bb)
  where
    r = v + W audio
    g = v * Y audio
    b = v * Z audio

    v = cos (radius uvN * X audio * 100 + time)
      + 0.9 *^ tan (X uvN * X audio * 10 + time)
    bb = Texture2D backBuffer ((rot (Z audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)



myColor2 = (0.1 *^ Vec4 (r, g, b, 1) + bb)
  where
    r = v + W audio
    g = v * Y audio
    b = v * Z audio

    v = cos (radius uvN * X audio * 100 + time)
      + 0.9 *^ sin (X uvN * X audio * 10 + time)
    bb = Texture2D backBuffer ((rot (Z audio * 0.5) (uvN * 0.95)) * 0.5 + 0.5)






















myColor = (0.1 *^ Vec4 (r, g, b, 1) + bb)
  where
    r = v + W audio
    g = v * Y audio
    b = v * Z audio

    v = v' * Z audio
    v' =recip $ tan  (Y uvN * X audio * 10 + tim)
      + 0.9 *^ sin (X bb  * X audio * 10 +tim )
    bb = Texture2D backBuffer ((rot (Z audio) (((Y audio *^ uvN)) * 0.9)) * 0.5 + 0.5)
    tim = time * 0.1


myColor3 = 0.1 *^ Vec4 (r,g ,b, 1) + bb
  where
    r = v' * W audio
    g = v' * Z audio
    b = v' * Y audio
    v' = product $ map v [0..10]
    v x = tan ((X audio + phi uvN') + time * 0.01 + W audio)
      where
        uvN' = rot time (Y audio *^ Vec2 (Y uvN , X uvN) + (Z audio *^ 0.2) * fromInteger (x))
    bb = Texture2D backBuffer (0.5 * (rot tim (uvN * 1.1)) + 0.5)
    tim = time * 0.1














mirror :: Vec2 -> Vec2
mirror v = Vec2 (abs $ X v, Y v)

myColor4 = 0.01 *^ Vec4 (r,g ,b, 1) + bb
  where
    r = v * X uvN
    g = v * Y uvN
    b = v
    v = radius (uvN - mouse) * (10 * X audio)
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror

myColor5 = 0.01 *^ Vec4 (r,g ,b, 1) + bb
  where
    r = v * X uvN
    g = v * Y uvN
    b = v
    v = radius (uvN - mouse) * (100 * X audio)
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (2 * pi / 3) . (*(0.90)) . mirror


spaceKandinsky = 0.01 *^ Vec4 (r,g ,b, 1) + 1.01 *^ bb
  where
    r = v * fract (X uvN * 100)
    g = v * fract (Y uvN * 100)
    b = v
    v = radius (uvN - mouse) * 10
      & \x -> 10 * sin x
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = rot (pi/2) . (*(0.5)) . mirror

dashiki= 0.01 *^ Vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    mul = 100
    r = v * fract (X uvN * mul)
    g = v * fract (Y uvN * mul)
    b = v
    tim = time/ 10e1
    v = radius (uvN - mouse + Vec2 (sin tim, cos tim)) * Y mouse
      & \x -> 10 * sin x
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (pi/4)
          . (*(0.99))

satanic = 0.01 *^ Vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    r = v
    g = v
    b = v
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (uvN - mouse + Vec2(sin tim, cos tim))
      & (\x -> 10 * x + (fromInteger n))
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (2 * pi / 5)
          . (*(0.99))

cottonCandy = 0.01 *^ Vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    g = v * Y audio * X uvN
    b = v * Z audio * fract (radius (uvN * 100))
    r = v * W audio * cos (radius (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (Vec2 (X uvN, (-1) * Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * W audio)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

cottonCandy2 = 0.01 *^ Vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * Y audio * X uvN
    r = 1 - v * Z audio * fract (radius (uvN * 100))
    g = 0.5 + v * W audio * cos (radius (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (Vec2 (X uvN, (-1) * Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot ( (-1 ) * pi/ 6 * W audio)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

cottonCandy3 = 0.01 *^ Vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 10 -  v * Y audio * X uvN
    r = v * Z audio * fract (radius (uvN * 100))
    g = v * W audio * cos (radius (uvN * 100))
    tim = time/ 10e1
    v = product $ map fn [0..1]
    fn n = Y (Vec2 (X uvN, Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 5)
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . mirror
          . rot (pi/ 6 * W audio)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

cottonCandy4 = 0.01 *^ Vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    r = v * X uvN
    g = v * Y uvN
    b = v
    v = tan $ radius (uvN - mouse) * X audio * 100
    bb = Texture2D backBuffer (0.5 * fn (uvN) + 0.5)
      where
        fn = id
          . mirror
          . rot (time * 0.001)
          . (^*(X audio & linexp (0, 1, 1, 0.9)))

white :: Vec4
white = vec (1, 1, 1, 1)

black :: Vec4
black = vec (0, 0, 0, 1)

test = select true black white

-- main = putStrLn . toGLSL $ gameOfLife
-- gameOfLife = Vec4 (v, v, v, 1)
--   where
--     v = select false 1 0

candyRoad = 0.01 *^ Vec4 (r,g ,b, 1) + 1.1 *^ bb
  where
    b = 1 -  v * Y audio * X uvN
    r = v * Z audio * fract (radius (uvN * 100))
    g = v * W audio * cos (radius (uvN * 100))
    v = product $ map fn [0..10]
    fn n = Y (Vec2 (X uvN, Y uvN))
      & (\x -> 10 * x + (fromInteger n) * 10)
      & tan
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+Vec2(0, -0.33))
          . mirror
          . rot (0.3* W audio* m)
          . (^*(X audio & linexp (0, 1, 1.1, 0.9)))
          . (+Vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)

candyRoad2 = 0.01 *^ Vec4 (r v,g v ,b v, 1) + 0.99 *^ bb
  where
    r :: Vec1 -> Vec1
    r v = v - tan (v * 0.001 + sin(tim + Y uvN)) ** 0.1

    g :: Vec1 -> Vec1
    g v = v - tan (v * 0.001 + sin(tim + X uvN)) ** 0.3

    b :: Vec1 -> Vec1
    b v = v - tan (v * 0.001 + sin(tim + radius uvN)) ** 0.4

    tim = time * 0.01

    v = (X audio & linexp (0, 1, 1, 100)) - radius (uvN - vec (0.5 * Y audio, -0.333)) * Y audio
      & (*(X audio & linexp (0, 1, 0.001, 1)))
      & (tan)
      & (cos)
      & (+(W audio * Y uvN))
      & (fract)
      & (*(Y audio & linexp (0, 1, 1, 10)))
      & (tan)
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn = id
          . (+Vec2(0, -0.33))
          . mirror
          . rot (0.3 * W audio* m)
          . (^*(X audio & linexp (0, 1, 1.1, 0.9)))
          . (+Vec2(0, 0.33))
    m = sin(time) & linexp (-1, 1, 0.5, 1)


candyRoad3 = 0.01 *^ Vec4 (r v, g v , b v, 1) + 0.99 *^ bb
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
    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.2) * W audio* m)
          . mirror
          . rot ((-0.2) * W audio* m)
          . (^*(X audio & linexp (0, 1, 1.1, 0.85)))
          . (+vec (-k, k))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = X audio & linexp (0, 1, 0.000001, 0.001)

candyRoad4 = 0.01 *^ Vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *Y audio * cos(X audio* 10 * sin(tim + X uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) **0.1

    tim = time * 0.01

    v = radius (uvN - vec (0.1, 0)) * Z audio
      & (*(X audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * (W audio+ X audio)* m)
          . (^*((X audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * W audio* m)
          . (+vec (-k, 0))
    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = Y audio & linexp (0, 1, 0.0000001, 0.001)

candyRoad5 = 0.01 *^ Vec4 (r v,g v ,b v, 1) + 0.99 *^ (bb)
  & (clamp 0 1)
  where
    r :: Vec1 -> Vec1
    r v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) ** 10

    g :: Vec1 -> Vec1
    g v = v *Y audio * cos(X audio* 10 * sin(tim + X uvN * 100)) ** 1

    b :: Vec1 -> Vec1
    b v = v *Y audio * cos(X audio * 10 * sin(tim + X uvN * 100)) **0.1

    tim = time * 0.01

    v = radius (uvN - vec (0.1, 0)) * Z audio
      & (*(X audio & linexp (0, 1, 1, 10e4)))
      & tan

    bb = Texture2D backBuffer (0.5 * fn uvN + 0.5)
      where
        fn :: Vec2 -> Vec2
        fn = id
          . rot ((-0.02) * (W audio+ X audio)* m)
          . (^*((X audio & linexp (0, 1, 1.25, 0.81))))
          . mirror
          . rot ((-0.2) * W audio* m)
          . (+vec (-k, 0))
          . (**0.999)
          . (sin)

    m = sin(time) & linexp (-1, 1, 0.5, 1)
    k = Y audio & linexp (0, 1, 0.0000001, 0.001)
-- TODO: make flash
main = putStrLn . toGLSL $ candyRoad5
