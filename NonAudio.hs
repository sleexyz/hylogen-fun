{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module NonAudio where

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


world :: Vec4
world = vec4 (r, g, b, 1)
  where
    gap = 10
    m = sin(time * 0.1) & linexp (-1, 1, 10e1, 10e10)
    ratemul = 0.5
    val   = cos(len uvN * m + time + sin(time + gap) * ratemul)
    val'  = cos(len uvN * m + time + sin(time + gap ** 2) * ratemul)
    val'' = cos(len uvN * m + time + sin(time + gap ** 3) * ratemul)
    r = val ** 2
    g = val' ** 2
    b = val'' ** 2


poop  = vec4 (r, g, b, 1)
  where
    tim = time * 0.1
      & \x -> tan x - sin x
    pos = vec2 (x_ uv - tim * 2, y_ uv - tim **2)
    val = sin $ len pos * 10 * (sin $ (x_ pos * y_ pos) /x_ audio)
    r = val ** 10
      + (x_ uv * y_ uv + sin (time ** 0.2 * w_ audio * 2))
    g = (1 * val **0.5)
      - (cos (x_ uv * y_ uv) + cos (time ** (0.5 * z_ audio) +x_ audio))
      * (sin (x_ uv * 10 + time))
    b = (1 * val ** 2)
      - (x_ uv * y_ uv + sin (time ** 0.1 +x_ audio))  - 1
      * (sin (x_ uv * 10 + time))


illusion =  vec4 (x, x, x, 1)
  where
    x = sin (time
             & (*0.1)
             & \x -> uvN
                   & rot (sin $time)
                   & \uv' -> sin (sqrt(x))*10 /x_ uv' - (sin(sqrt x)) * 10 /y_ uv')
      & \x -> 1/ sqrt x

coolio = vec4 (v, v, v, 1)
  where
    circle uv' = 1 - len uv'' * 10
      where
        uv'' = cos (uv' * 10)
    m = vec2 (x_ mouse, y_ mouse)

    circles = product $ map fn [0..10]
      where
        fn x = circle (uvN * m * fromInteger x)
    v = circles

cooooool =  vec4 (v, v, v, 1)
  where
    circle uv' = tan $ 1 - len uv'' * 10
      where
        uv'' = cos (uv' * 10)
    m = vec2 (x_ mouse, y_ mouse)

    circles = product $ map fn [0..11]
      where
        fn x = circle (uvN * m * copy (fromInteger x + 0.001*time))
    v = circles


comp = (0.01 * vec4 (v, v, v, 1)  + bb)
  where
    v = product $ circle . fromInteger <$>  [0.. 2]
    circle x = (10 - 10 * len (sin (uvN * 10 + 0.2 *  x) - m))
      & \x -> x / 4


    bb = texture2D backBuffer ((uvN * 0.95) * 0.5 + 0.5)
    m = vec2 (x_ mouse, y_ mouse)


thang = (0.01 * vec4 (v, v, v, 1)  + bb)
  where
    v = product $ circle . fromInteger <$>  [0.. 4]
    circle x = (10 - 10 * len (sin (uvN * 10 + 0.2 *  x ) + 0.5 * vec2 (cos $ time * 10, sin $  time * 10)))
      & \x -> x / 5


    bb = texture2D backBuffer ((uvN * (vec2(x_ audio,x_ audio))) * 0.5 + 0.5)
    m = vec2 (x_ mouse, y_ mouse)


graph :: Vec4
graph = v
  where
    resolution = 500
    isDrawn =x_ uv `lt` (1 / resolution)

    v = sel isDrawn fresh old
    fresh = vec4 (v, v, v, 1)
      where
        v = sel (abs ((y_ uvN - y_ mouse)) `lt` 0.01) 1 0
        mul = 8

    old = texture2D backBuffer pos
      where
        pos = (uvN * 0.5 + 0.5) - vec2 (1/resolution, 0)


opaque :: Vec4 -> Vec4
opaque v = vec4 (x_ v, y_ v, z_ v, 1)

coolio2 :: Vec4
coolio2 = circles
  where

    circles = foldr (mix 0.1 . fn) 0 [0..15]
      where
        fn x = clamp 0 1 $ vec4 (w, w, w, 1)
          where
            w = circle (uvN * fromInteger x * 0.3)
    circle uv' = cos $  len (cos (uv' * 10)) * 10 + time

thanger :: Vec4
thanger = mix 0.2 fresh bb
  where
    fresh = (sum $ map fn [0..10]) + bars

      where
        bars = vec4 (v, v, v, 1)
          where
            v = cos (x_ uv * 10)
        fn x = vec4 (v, v, v, 1)
          where
            v = (cos (len (uvN - mouse) * 10 + 3 * fromInteger x))
    bb = texture2D backBuffer (pos)
      where
        pos = uvN
          & (\x -> x * 0.9)
          & (rot (pi/2))
          & (\x -> vec2 (x_ x, y_ x))
          & (\x -> x * 0.5 + 0.5)

notgameOfLife :: Vec4
notgameOfLife = vec4 (v, v, v, 1)
  where
    res = 1000
    v = sel alive 1 0
    alive = sum [rule1, rule2, rule3, rule4]

    neighbors = [ (-1,-1), (-1, 0), (-1, 1)
                , ( 0,-1),          ( 0, 1)
                , ( 1,-1), ( 1, 0), ( 1, 1)
                ]

    numAlive :: Vec1
    numAlive = sum $ map getVal neighbors
      where
        getVal offset =x_ $ texture2D backBuffer
          $ uv - vec2 offset ^* (1/res)

    wasAlive = val `gt` 0
      where
        val =x_ $ texture2D backBuffer $ uv

    rule1 = wasAlive * (numAlive `geq` 2)
    rule2 = wasAlive * (numAlive `leq` 3)
    rule3 = numAlive `eq` 1
    rule4 = (max_ (x_ dist) (y_ dist)) `leq` (1/res)
      where
        dist = uvN
        -- dist = (*res) . floor_ . (*(1/res)) $ uvN - mouse

notgameOfLifeEither :: Vec4
notgameOfLifeEither = vec4 (v, v, v, 1)
  where
    res = 500
    v = sel alive 1 0
    alive = sum [rule1, rule2, rule3, rule4]

    neighbors = [ (-1,-1), (-1, 0), (-1, 1)
                , ( 0,-1),          ( 0, 1)
                , ( 1,-1), ( 1, 0), ( 1, 1)
                ]

    numAlive :: Vec1
    numAlive = sum $ map getVal neighbors
      where
        getVal offset =x_ $ texture2D backBuffer
          $ uv - vec2 offset ^* (1/res)

    wasAlive = val `gt` 0
      where
        val =x_ $ texture2D backBuffer $ uv

    rule1 = wasAlive * (numAlive `geq` 2)
    rule2 = wasAlive * (numAlive `leq` 3)
    rule3 = numAlive `eq` 3
    rule4 = taxicab dist `leq` (1/res)
      where
        dist = uvN - mouse

taxicab :: Vec2 -> Vec1
taxicab x = max_ (abs $x_ x) (abs $ y_ x)


downsample :: (Veccable a) => Vec a -> Vec a -> Vec a
downsample a x = floor_ (a*x) / a


gameOfLifeTake1 :: Vec4
gameOfLifeTake1 = vec4 (v, v, v, 1)
  where
    res = 50

    v = sel alive 1 0
    alive = sum [rule1, rule3, rule4]

    neighbors = [ (-1,-1), (-1, 0), (-1, 1)
                , ( 0,-1),          ( 0, 1)
                , ( 1,-1), ( 1, 0), ( 1, 1)
                ]

    numAlive :: Vec1
    numAlive = sum $ map getVal neighbors
      where
        getVal offset =x_ $ texture2D backBuffer
          $ downsample res
          $ (uvN * 0.5 + 0.5) - vec2 offset ^* (1/res)

    wasAlive = val `gt` 0
      where
        val =x_ $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

    rule1 = wasAlive * ((numAlive `geq` 2) * (numAlive `leq` 3))
    rule3 = numAlive `eq` 3
    rule4 = taxicab dist `lt` (1/res)
      where
        dist = (downsample res uvN) - mouse


-- neighbors :: (Eq a, Num a) => [[a]]
-- neighbors = filter (/=[0, 0]) $ do
--   x <- [-1, 0, 1]
--   y <- [-1, 0, 1]
--   return [x, y]

gridTest :: Vec4
gridTest = vec4 (v, v, v, 1)
  where
    res = 4
    n = 1
    duv = downsample res uv
    v = sel ((x_ duv `lt` ((n+1)/res)) * (x_ duv `geq` ((n)/res))) (x_ uv) 0

gridTest2 :: Vec4
gridTest2 = vec4 (v, v, v, 1)
  where
    res = 5

    duv = downsample res uvN

    maxi= downsample res $ mouse + 1/res
    mini= downsample res $ mouse

    draw = product [x_ duv `lt`x_ maxi
                   ,x_ duv `geq`x_ mini
                   , y_ duv `lt` y_ maxi
                   , y_ duv `geq` y_ mini
                   ]
    v = sel draw 1 0

drawingApp:: Vec4
drawingApp = vec4 (v, v, v, 1)
  where
    res = 10

    duv = downsample res uvN

    maxi= downsample res $ mouse + 1/res
    mini= downsample res $ mouse

    drawWithMouse = product [ duv `lt` maxi
                            , duv `geq` mini
                            ]

    v = sel drawWithMouse 1
      $ sel alive 1 0
      where
        alive = wasAlive

        wasAlive = val `gt` 0
          where
            val =x_ $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

drawingApp2:: Vec4
drawingApp2 = vec4 (v, v, v, 1)
  where
    res = 100

    duv = downsample res uvN

    maxi= downsample res $ mouse + 1/res
    mini= downsample res $ mouse

    drawWithMouse = product [ duv `lt` maxi
                            , duv `geq` mini
                            ]

    v = sel drawWithMouse 1
      $ sel alive invright 0
      where
        invright = (x_ $ texture2D backBuffer $ (((uvN * res  + 1)/res) * 0.5 + 0.5))
          & (\x -> (x * res + 1)/res)

        alive = wasAlive

        wasAlive = val `gt` 0
          where
            val =x_ $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

notSupposedToBeGameOfLife :: Vec4
notSupposedToBeGameOfLife = vec4 (v, v, v, 1)
  where
    res = 100

    duv = downsample res uvN

    maxi= downsample res $ mouse + 1/res
    mini= downsample res $ mouse

    drawWithMouse = product [ duv `lt` maxi
                            , duv `geq` mini
                            ]

    v = sel drawWithMouse 1
      $ sel alive 1 0
      where
        alive = sum [rule1, rule3]

        neighbors = [ (-1,-1), (-1, 0), (-1, 1)
                    , ( 0,-1),          ( 0, 1)
                    , ( 1,-1), ( 1, 0), ( 1, 1)
                    ]

        numAlive :: Vec1
        numAlive = sum $ map getVal neighbors
          where
            getVal offset =x_ $ texture2D backBuffer
              $ id             ( uvN
                                 & (\x -> (res * x + vec2 offset) / res)
                                 & (\x -> x * 0.5 + 0.5)
                               )

        wasAlive = val `gt` 0
          where
            val =x_ $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

        rule1 = wasAlive * ((numAlive `eq` 2) + (numAlive `eq` 3))
        rule3 = negate wasAlive * numAlive `eq` 3

gameOfLifeTake2 :: Vec4
gameOfLifeTake2 = vec4 (v, v, v, 1)
  where
    res = 1000

    duv = downsample res uvN

    maxi= downsample res $ mouse + 1000/res
    mini= downsample res $ mouse

    drawWithMouse = product [ duv `lt` maxi
                            , duv `geq` mini
                            ]

    v = sel drawWithMouse 1
      $ sel alive 1 0
      where
        alive = sum [rule1, rule3]

        neighbors = [ (-1,-1), (-1, 0), (-1, 1)
                    , ( 0,-1),          ( 0, 1)
                    , ( 1,-1), ( 1, 0), ( 1, 1)
                    ]

        numAlive :: Vec1
        numAlive = sum $ map getVal neighbors
          where
            getVal offset =x_ $ texture2D backBuffer
              $ id             ( uvN
                                 & (\x -> (res * x + vec2 offset) / res)
                                 & (\x -> x * 0.5 + 0.5)
                               )

        wasAlive = val `gt` 0
          where
            val =x_ $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

        rule1 = wasAlive * ((numAlive `eq` 2) + (numAlive `eq` 3))
        rule3 = negate wasAlive * numAlive `eq` 3

gameOfLifeTake3 :: Vec4
gameOfLifeTake3 = vec4 (v, v, v, 1)
  where
    res = copy $ floor_ $x_ resolution /2

    duv = downsample res uvN

    maxi= downsample res $ mouse + 50/res
    mini= downsample res $ mouse - 50/res

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
            getVal offset =x_ $ texture2D backBuffer
              $ id             ( uvN
                                 & (\x -> (res * x + vec2 offset) / res)
                                 & (\x -> x * 0.5 + 0.5)
                               )

        wasAlive = val `gt` 0
          where
            val =x_ $ texture2D backBuffer $ (uvN * 0.5 + 0.5)

        rule1 = wasAlive * ((numAlive `eq` 2) + (numAlive `eq` 3))
        rule3 = negate wasAlive * numAlive `eq` 3


sigmoid :: Vec1 -> Vec1
sigmoid x = recip (1 + exp (negate x))

selSmooth :: Vec1 -> Vec1 -> Vec1 -> Vec1
selSmooth choice bw x = sigmoid (x - choice) * sigmoid (negate x - choice)

testSelSmooth :: Vec4
testSelSmooth = vec4 (v, v, v, 1)
  where
    -- v = selSmooth 0.01 1 (y_ uv)
    v = sigmoid (y_ uvN * 10)

testDotProduct :: Vec4
testDotProduct = vec4 (v, v, v, 1) * vec4 (0.5, 0.2, 1, 1) * 2
  where
    circle r uv = sel inside 1 0
      where
        inside = len uv `lt` r

    sphere :: Vec1 -> Vec2 -> Vec1
    sphere r uv = max_ 0 $ sqrt (r*r - (uv <.> uv))
    v = sum [ sphere 0.5 (uvN)
            , 0.2
            , lambert
            ]
    lambert = max_ ((vec3 (uvN, sphere 0.5 uvN) <.> vec3 (mouse, 1))) 0

testUnswizz :: Vec4
testUnswizz = sum [ vec4 (0, 0, 0, 0)
                  , vec4 (vec2 (0, 0), 0, 0)
                  , vec4 (0, vec2 (0, 0))
                  ]

infixl 5 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap


sdf0 :: Vec4
sdf0 = vec4 (color, color, color, 1)
  where
    eye = vec3 (0, 0, -1)

    right = vec3 (1, 0, 0)
    up = vec3 (0, 1, 0)

    u =x_ uvN
    v = y_ uvN

    ro = u *^ right + v *^ up -- we embed our textures into 3-space
    rd = eye

    black = 0

    -- length :: _

    rayMarch :: Int -> Vec1
    rayMarch n = snd $ (!!n) $ iterate fn (0, 0)
      where
        fn :: (Vec1, Vec1) -> (Vec1, Vec1)
        fn (t, c) = (t + d, c')
          where
            d = len ((ro + t *^ rd) - vec3 (mouse, 0)) - 0.1
            c' = sel (d `lt` 0.1) 1 0
            -- TODO: break!

    color = rayMarch 2

sdf1 :: Vec4
sdf1 = vec4 (color, color, color, 1)
  where
    eye = vec3 (0, 0, -1)

    right = vec3 (1, 0, 0)
    up = vec3 (0, 1, 0)

    u =x_ uvN
    v = y_ uvN

    ro = u *^ right + v *^ up -- we embed our textures into 3-space
    rd = eye

    black = 0

    -- length :: _

    rayMarch :: Int -> Vec1
    rayMarch n = snd $ (!!n) $ iterate fn (0, 0)
      where
        fn :: (Vec1, Vec1) -> (Vec1, Vec1)
        fn (t, c) = (t + d, c')
          where
            d = len ((ro + t *^ rd) - vec3 (mouse, 0)) - 0.1
            c' = sel (d `lt` 0.1) 1 0

    color = rayMarch 7


-- TODO: as of May 1, quadradic compilation times wrt i
-- Stack overflows at 10
bench :: Int -> Vec4
bench i = vec4 (color, color, color, 1)
  where
    eye = vec3 (0, 0, -1)

    right = vec3 (1, 0, 0)
    up = vec3 (0, 1, 0)

    u =x_ uvN
    v = y_ uvN

    ro = u *^ right + v *^ up -- we embed our textures into 3-space
    rd = eye

    black = 0

    -- length :: _

    rayMarch :: Int -> Vec1
    rayMarch n = snd $ (!!n) $ iterate fn (0, 0)
      where
        fn :: (Vec1, Vec1) -> (Vec1, Vec1)
        fn (t, c) = (t + d, c')
          where
            d = len ((ro + t *^ rd) - vec3 (mouse, 0)) - 0.1
            c' = sel (d `lt` 0.1) 1 0

    color = rayMarch i

red :: Vec3
red = vec3 (1, 0, 0)

green :: Vec3
green = vec3 (0, 1, 0)

-- TODO: implement monoidal append


square :: Vec2 -> Vec4
square v = sel (len(uvN - v) `lt` (copy $ z_ audio * 0.2)) (vec4 (color, 0.99)) (vec4(0, 0, 0, 0))
  where
    color = mix (sin time * 0.5 + 0.5) green red


-- infixl 7 `over`

over :: Vec4 -> Vec4 -> Vec4
over x y = mix (a) x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - w_ x'

opacity :: Vec1 -> Vec4 -> Vec4
opacity f x = vec4 (xyz_ x, f)

field :: Integer -> [Vec2]
field i = [vec2 (fromInteger x,fromInteger y) | x <- rng, y <- rng]
  where
    rng = [(-i)..i]

testNewVer :: Vec4
testNewVer = color
  where
    bb = texture2D backBuffer (f uvN)
    f = id
      >>>(*0.9)
      >>>(*0.5) >>>(+0.5)

    color = foldr over (bb) myfield
      where
        myfield = field 2
          <&> (*0.2)
          <&> (rot $ time * 0.1)
          <&> fuzz
    fuzz v = vec4 (black, 1 - len (shman 5 $ uvN - v) * (sin time * 4 + 10))
    shman n = (*n) >>> tan >>> (/n)

oscColor :: Vec3
oscColor = vec3 (osc1, osc2, osc3)

testNewVer1 :: Vec4
testNewVer1 = color
  where
    bb = texture2D backBuffer (f uvN)
    f = id
      >>>(^*(0.9))
      >>>(\x -> vec2 (abs (x_ x), y_ x))
      >>>(rot (time * 0.1))
      >>>(*0.5) >>>(+0.5)

    color = foldr over (bb) myfield
      where
        myfield = field 1
          <&> (*0.2)
          <&> (rot $ time * 0.1)
          <&> fuzz
    fuzz v = vec4 (oscColor, 1 - len (uvN -  v) * (sin time * 4 + 10))
    shman n = (*n) >>> sin >>> (/n)


output = toProgram testNewVer1
