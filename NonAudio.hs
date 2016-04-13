{-# LANGUAGE NoMonomorphismRestriction #-}
module NonAudio where

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


graph :: Vec4
graph = v
  where
    resolution = 500
    isDrawn = X uv `lt` (1 / resolution)

    v = select isDrawn fresh old
    fresh = Vec4 (v, v, v, 1)
      where
        v = select (abs ((Y uvN - Y mouse)) `lt` 0.01) 1 0
        mul = 8

    old = Texture2D backBuffer pos
      where
        pos = (uvN * 0.5 + 0.5) - Vec2 (1/resolution, 0)


opaque :: Vec4 -> Vec4
opaque v = Vec4 (X v, Y v, Z v, 1)

coolio2 :: Vec4
coolio2 = circles
  where

    circles = foldr (mix 0.1 . fn) 0 [0..15]
      where
        fn x = clamp 0 1 $ Vec4 (w, w, w, 1)
          where
            w = circle (uvN * fromInteger x * 0.3)
    circle uv' = cos $  radius (cos (uv' * 10)) * 10 + time

thanger :: Vec4
thanger = mix 0.2 fresh bb
  where
    fresh = (sum $ map fn [0..10]) + bars

      where
        bars = Vec4 (v, v, v, 1)
          where
            v = cos (X uv * 10)
        fn x = Vec4 (v, v, v, 1)
          where
            v = (cos (radius (uvN - mouse) * 10 + 3 * fromInteger x))
    bb = Texture2D backBuffer (pos)
      where
        pos = uvN
          & (\x -> x * 0.9)
          & (rot (pi/2))
          & (\x -> Vec2 (X x, Y x))
          & (\x -> x * 0.5 + 0.5)



main = putStrLn $ toGLSL $ thanger
