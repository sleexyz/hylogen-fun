{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Corp where
-- 05/21 @ brooklyn safehouse, job fair
-- w/ corp industries incorparated and fratt institute

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
                
select = sel


infixl 5 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap


red :: Vec3
red = vec3 (1, 0, 0)

green :: Vec3
green = vec3 (0, 1, 0)

black :: Vec3
black = vec3 (0, 0, 0)

white :: Vec3
white = vec3 (1, 1, 1)

-- TODO: implement monoidal append




-- infixl 7 `over`

over :: Vec4 -> Vec4 -> Vec4
over x y = mix (a) x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - w_ x'

-- TODO: rename setOpacity
opacity :: Vec1 -> Vec4 -> Vec4
opacity f x = vec4 (x_ x, y_ x, z_ x, f)

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
oscColor = vec3 (osc1, osc2, osc3) ^* osc5

square :: Vec2 -> Vec4
square v = select (len(uvN - v) `lt` (copy $ y_ audio * 0.01)) (vec4 (color, 1)) (vec4(0, 0, 0, 0.001)) 
  where
    color = oscColor

testNewVer1 :: Vec4
testNewVer1 = color
  where
    bb = texture2D backBuffer (f uvN)
    f = id
    -- f x = x
    --   & (^*(x_ audio + osc7))
    --   & (\x -> vec2 (absx_ x, abs x_ y))
    --   & (rot (pi + 0.2 *  y_ audio))
    --   & (\x -> x - mouse)
    --   & (*0.5) & (0.5)

    color = foldr over bb myfield 
      where
        myfield = field 1
          <&> (^*(osc6 * x_ audio ))
          <&>(rot (time))
          <&> solid

    shman n = (*n) >>> cos>>> (/n)
    solid v = select (abs (uvN - v) `lt` (copy $ y_ audio * osc8 * 0.1)) (vec4 (oscColor, 0.9)) (vec4(0, 0, 0, 0))

audioGraph2 :: Vec4
audioGraph2 = v
  where
    resolution = 100
    isDrawn = abs (x_ uvN  -  (1 / resolution)) `lt` (1/resolution)

    v = select isDrawn fresh old
    fresh = vec4 (v *^ oscColor, 1)
      where

        v = z_ audio * y_ uv
          & (*4)
          & fract

    old = texture2D backBuffer pos
      where
        pos = uvN
          & (\x -> vec2 (abs (x_ x), abs (y_ x)))
          & (\x -> vec2 (x_ x, y_ x * (x_ audio & linexp (0, 1, 0.8, 1.1))))
          & (\x -> x - vec2 (1/resolution * signum (x_ uvN), 0))
          & (rot (x_ audio * (osc8)))
          & (\x -> x * 0.5 + 0.5)

blah :: Vec4
blah = color
  where

    color = testNewVer1
    v = 10 - len uvN

    shman f n = (*n) >>> f >>> (/n)

shiet :: Vec4
shiet = color
  where
    bb = texture2D backBuffer (f uvN)
    f = id
      >>> (\x -> vec2 (abs $ x_ x, y_ x))
      >>> (\x -> x ^* x_ audio + 0.5)
      >>>(rot (0.5 *  y_ audio + time))
      >>>(*0.5) >>>(+0.5)
    color = vec4 (oscColor ^* v, mix osc8 v osc8) `over` bb
      where
       v = (clamp 0 1) . tan . cos
         $ 30 * (1 - len uvN * x_ audio * y_ uvN * sin time)



output = toProgram audioGraph2
