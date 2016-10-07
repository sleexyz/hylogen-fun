{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Jam_2016_10_05 where

-- tempo sync

import Hylogen.WithHylide
import Data.Profunctor

pattern Vec2 x y <- (toList -> [x, y])
pattern Vec3 x y z <- (toList -> [x, y, z])
pattern Vec4 x y z w <- (toList -> [x, y, z, w])

type Optic p s t a b = p a b -> p s t
type Optic' p a b = p a b -> p a b
type Iso s t a b = forall p. (Profunctor p) => Optic p s t a b
type Iso' a b = forall p. (Profunctor p) => Optic p a b a b
type Fold r s t a b = Optic (Forget r) s t a b
type Getter s t a b = Fold a s t a b
view :: forall s t a b. Getter s t a b -> s -> a
view l = runForget (l (Forget id))

norm :: forall a b. (Floating a, Floating b) => Iso a b a b
norm = dimap (\x -> x * 0.5 + 0.5) (\x -> x * 2 - 1)

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )


over :: Vec4 -> Vec4 -> Vec4
over x y = mix a x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - w_ x'

rep :: Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

inMiddle :: Veccable v => (Vec v -> Vec v) -> Vec v -> Vec v
inMiddle = dimap (+0.5) (`subtract`0.5)

mirrorY :: Vec2 -> Vec2
mirrorY (Vec2 x y) =  vec2 (inMiddle abs x, y)

mirrorX :: Vec2 -> Vec2
mirrorX (Vec2 x y) =  vec2 (inMiddle abs x, y)

rgbF :: Vec1 -> Optic' (->) Vec2 Vec4
rgbF m q pos = vec4 (r, g, b, a)
  where
    r = q (pos + copy m) & x_
    g = q pos & y_
    b = q (pos - copy m) & z_
    a = q pos & w_


colormap :: (Vec1 -> Vec1) -> Vec4 -> Vec4
colormap f v = vec4 (f r, f g, f b, a)
  where
    r = x_ v
    g = y_ v
    b = z_ v
    a = w_ v

hsv :: Iso' Vec4 Vec4
hsv = dimap rgb2hsv hsv2rgb

desaturate = hsv (\v -> vec4(x_ v , y_ v, z_ v, w_ v))

output = toProgram $ rgb
  & mix (0.1) bb
bb = bbEffects (texture2D backBuffer) uvN
bbEffects x = x
  & rgbF 0.01
  & rmap desaturate
  & lmap (view norm)
  & lmap (*0.9)
  & lmap (rot (pi/3))

rgb = vec4 (v, v, v, 1)
v = cos (sin . (*0.25) $  time) * mask
mask =  x_ audio
 & (*10)
 & (*shade)
shade = xshade - yshade
xshade = y_ uvN
 &(*10)
 & sin
 & cos
yshade = x_ uvN
 & (\x -> x * (0.5 - y_ audio) * 10)
 & clamp (-1) 1
 & cos . abs . cos
