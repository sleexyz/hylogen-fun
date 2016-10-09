{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

module SDF where

import Hylogen.WithHylide

sphere :: Vec3 -> Vec1 -> Vec3 -> Vec1
sphere spherePos radius eyePos = len (eyePos - spherePos) - radius

black = vec3 (0, 0, 0)
white = vec3 (1, 1, 1)


rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * (x_ a)
                   + sin phi * (y_ a)
                 , (-1) * sin phi * (x_ a)
                   + cos phi * (y_ a)
                 )

-- 2016-05-23
-- got a circle to show
raymarch0 =  id
  . clamp 0 1
  . (\x -> vec4(x, 1))
  . fst
  $ foldr fn  (black, 0) (fromInteger <$> [0..10])
  where
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)

    ro = vec3 (uvN, 0)
    rd = eye -- ortho

    -- ro = 0
    -- rd = eye -- ortho

    fn :: Vec1 -> (Vec3, Vec1) -> (Vec3, Vec1)
    fn i (color, t) =
      let p = ro + rd ^* t
          d = sphere (vec3 (0, 0, 0)) 0.5 p
          newColor = vec3(1, 0, 1)
      in ( sel (d `lt` 0.000001) newColor color
         , t + d
         )



-- TODO: implement a Selectable typeclass
-- class Selectable a where
--   toExpr f




-- 2016-05-23
-- what up!
raymarch1 = id
  . clamp 0 1
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    eye = vec3 (0, 0, -1 * (mouse&y_) * 1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)

    ro = vec3 (uvN, 0)
    rd = eye -- ortho

    maxSteps :: Integer
    maxSteps = 32


    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = len (p - vec3(0, 0, -10))  - 0.3
          newColor = white ^* (i/fromInteger maxSteps)
          mini = 0.000001
      in ( sel continue
           (sel (d `lt` mini)
             newColor
             color
           )
           color
         , sel continue
           (sel (d `lt` mini)
             t
             t + d
           )
           t
         , sel continue
           (sel (d `lt` mini)
             false
             true
           )
           continue
         )

-- 2016-05-23
raymarch2 = id
  . clamp 0 1
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)

    ro = vec3 (uvN, 1)
    -- rd = eye
    rd = eye ^* (0.8) + right ^* x_ uvN + up ^* y_ uvN --perspective!


    maxSteps :: Integer
    maxSteps = 32

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    sdf :: Vec3 -> Vec1
    sdf p = sphere (vec3(mouse, -2)) (1.5) (p)
            `min_` plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p
            `min_` plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p
            `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p




    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = mix i' (white * 0.2) white

          cond = (d `leq` 0.0001)
      in ( sel continue
           (sel cond
             newColor
             color
           )
           color
         , sel continue
           (sel cond
             t
             t + d
           )
           t
         , sel continue
           (sel cond
             false
             true
           )
           continue
         )

-- 2016-05-23
raymarch3 = id
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)

    ro = vec3 (uvN, 1)
    -- rd = eye
    rd = eye ^* (0.9) + right ^* x_ uvN + up ^* y_ uvN --perspective!


    maxSteps :: Integer
    maxSteps = 64

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    sdf :: Vec3 -> Vec1
    sdf p = sphere (vec3(0, 0, -1)) (1) (repeat (copy $ y_ mouse * 10) p)
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p
            -- `min_` plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            -- `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p




    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9)

          cond = (d `lt` 0.000001)
      in ( sel continue
           (sel cond
             newColor
             color
           )
           color
         , sel continue
           (sel cond
             t
             t + d
           )
           t
         , sel continue
           (sel cond
             false
             true
           )
           continue
         )

-- 2016-05-23
raymarch4 = id
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)

    n = sin (time * 0.1)
        & ( id
            . (*0.5) . (+0.5)
            . (*(500))
          )

    ro = vec3 (uvN, negate n)
    -- rd = eye
    rd = eye ^* (y_ mouse) + right ^* x_ uvN + up ^* y_ uvN --perspective!

    -- implement normalize


    maxSteps :: Integer
    maxSteps = 64

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    sdf :: Vec3 -> Vec1
    sdf p = sphere (vec3(0, 0, -1)) (1) (repeat (copy $  sin (time * 0.1) * 2 + 4) p)
            -- `max_` (plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p)
            -- `min_` (plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p)
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            -- `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p




    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9)

          cond = (d `lt` 0.000001)
      in ( sel continue
           (sel cond
             newColor
             color
           )
           color
         , sel continue
           (sel cond
             t
             t + d
           )
           t
         , sel continue
           (sel cond
             false
             true
           )
           continue
         )

-- TODO: do a performance comparison

-- 2016-05-25
raymarch5 = id
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)

    n = sin (time * 0.1)
        & ( id
            . (*0.5) . (+0.5)
            . (*(100))
          )

    -- ro = vec3 (rot time 1 + rot time uvN ^* 10, n)
    ro = vec3 (rot (time * 0.5) 1, n)
      -- & normalize
    -- rd = eye
    rd = eye ^* 0.8 + right ^* x_ uvN + up ^* y_ uvN --perspective!
      & coolio
      & normalize

    coolio x = vec3 (y&x_, x&y_, y&y_)
      where
        y = rot (time * 0.1) $ vec2 (x&x_, x&z_)

  -- todo: fix thing negative thing

    -- implement normalize


    maxSteps :: Integer
    maxSteps = 32

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    sdf :: Vec3 -> Vec1
    -- sdf p = sphere (vec3(0, 0, -1)) (1) (repeat (copy $  sin (time * 0.1) * 2 + 4) p)
    sdf p = sphere (vec3(0, 0, -1)) (1) (repeat (copy $  2 + 4) p)
            -- `max_` (plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p)
            -- `min_` (plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p)
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            -- `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p




    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9)

          -- cond = (d `lt` 0.000001)
          cond = (d `lt` 0.0001)
      in ( sel continue
           (sel cond
             newColor
             color
           )
           color
         , sel continue
           (sel cond
             t
             t + d
           )
           t
         , sel continue
           (sel cond
             false
             true
           )
           continue
         )

-- 2016-05-26
raymarch6 = id
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)

    n = sin (time * 0.1)
        & ( id
            . (*0.5) . (+0.5)
            . (*(100))
          )

    -- ro = vec3 (rot time 1 + rot time uvN ^* 10, n)
    ro = vec3 (rot (time * 0.5) 1, n)
      -- & normalize
    -- rd = eye
    rd = eye ^* 0.8 + right ^* x_ uvN + up ^* y_ uvN --perspective!
      & coolio
      & normalize

    coolio x = vec3 (y&x_, x&y_, y&y_)
      where
        y = rot (time * 0.1) $ vec2 (x&x_, x&z_)

  -- todo: fix thing negative thing

    -- implement normalize


    maxSteps :: Integer
    maxSteps = 32

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    sdf :: Vec3 -> Vec1
    -- sdf p = sphere (vec3(0, 0, -1)) (1) (repeat (copy $  sin (time * 0.1) * 2 + 4) p)
    sdf p = sphere (vec3(0, 0, -1)) (1) (repeat (copy $  2 + 4) p)
            -- `max_` (plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p)
            -- `min_` (plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p)
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            -- `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p




    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9)

          -- cond = (d `lt` 0.000001)
          cond = (d `lt` 0.0001)
      in ( sel continue
           (sel cond
             newColor
             color
           )
           color
         , sel continue
           (sel cond
             t
             t + d
           )
           t
         , sel continue
           (sel cond
             false
             true
           )
           continue
         )

over :: Vec4 -> Vec4 -> Vec4
over x y = mix (a) x' y'
  where
    x' = clamp 0 1 x
    y' = clamp 0 1 y
    a = 1 - x'&w_

-- 2016-05-27
-- vaaaape
raymarch7 = id
  . (\x -> x `over` bb)
  . (\x -> vec4(x, x&z_))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    bb = texture2D backBuffer (f uvN)
      where
        f = id
          . (*2)
          . (*0.5) . (+0.5)
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)


    ro = vec3 (rot (time * 0.15) uvN, negate time)
    rd = eye ^* 0.8 + right ^* (uvN&x_) + up ^* (uvN&y_) --perspective!
      & (\x -> vec3 (rot (time * 0.1) (vec2(x&x_, x&y_)), x&z_))
      & normalize


    maxSteps :: Integer
    maxSteps = 16

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    box :: Vec3 -> Vec3 -> Vec3 -> Vec1
    box boxPos dim p = len (max_ (abs (p - boxPos) - dim) 0)

    -- return length(max(abs(p)-b,0.0));

    sdf :: Vec3 -> Vec1
    -- sdf p = (box (vec3(0, 0, -2.5)) (copy $ audio&x_) (repeat 5 p))
    sdf p = sphere (vec3(mouse, -2)) (1) (repeat 5  p )
    -- sdf p = sphere (vec3(mouse, -2)) (1) ( p )
            -- `min_` (plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p)
            -- `min_` (plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p)
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            -- `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p


    gradient :: (Vec3 -> Vec1) -> (Vec3 -> Vec3)
    gradient sf x = vec3 (partialX, partialY, partialZ)
      where
        partialX = (sf (vec3((x&x_) + epsilon, x&y_, x&z_)) - sf x) ^/ epsilon
        partialY = (sf (vec3(x&x_, (x&y_) + epsilon, x&z_)) - sf x) ^/ epsilon
        partialZ = (sf (vec3(x&x_, x&y_, (x&z_) + epsilon)) - sf x) ^/ epsilon
        epsilon = 0.00001



    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps

-- vec4 shade(vec3 p)
-- {
--     vec3 normal = getNormal(p);
--     vec3 lightDir = normalize(lightPosition - p);
--     float LightIntensity = lightColor * dot(normal, lightDir);
--     return getReflectance(p) * lightIntensity;
-- }
          shade sdf p = lightColor ^* (normal <.> lightDir)
            where
              normal = gradient sdf p
              lightColor = vec3(sin. (*i') . (*10) $(len uvN), 0.3, 0.9)
              lightPosition = vec3 (0, 1, 0)
              lightDir = normalize (lightPosition - p)

          newColor = (mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9))
            * shade sdf p

          cond = (d `lt` 0.001)
      in ( sel continue
           (sel cond
             newColor
             color
           )
           color
         , sel continue
           (sel cond
             t
             t + d
           )
           t
         , sel continue
           (sel cond
             false
             true
           )
           continue
         )

-- 2016-05-27
raymarch8 = id
  -- . (\x -> x `over` bb)
  . (\x -> vec4(x, 1))
  -- . (\x -> vec4(x, x&z_))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where
    bb = texture2D backBuffer (f uvN)
      where
        f = id
          -- . (*0.5)
          . (*0.5) . (+0.5)
    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)


    ro = vec3 (rot (time) uvN, tan (time * (-0.1)))
    rd = eye ^* 0.8 + right ^* (uvN&x_) + up ^* (uvN&y_) --perspective!
      & (\x -> vec3 (rot (time * 0.1) (vec2(x&x_, x&y_)), x&z_))
      & normalize


    maxSteps :: Integer
    maxSteps = 32

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    box :: Vec3 -> Vec3 -> Vec3 -> Vec1
    box boxPos dim p = len (max_ (abs (p - boxPos) - dim) 0)

    -- return length(max(abs(p)-b,0.0));

    sdf :: Vec3 -> Vec1
    -- sdf p = (box (vec3(0, 0, -2.5)) (copy $ audio&x_) (repeat 5 p))
    -- sdf p = sphere (vec3(mouse, -2)) (1) (repeat 5  p )
    sdf p = (box (vec3(mouse, -2.5)) (0.4) (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(5,0.1, 0.1)) (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(0.1,5, 0.1)) (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(0.1,0.1, 5)) (f p))
      where
        f = repeat 5
        -- f = id

    -- sdf p = sphere (vec3(mouse, -2)) (1) ( p )
            -- `min_` (plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p)
            -- `min_` (plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p)
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            -- `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p


    gradient :: (Vec3 -> Vec1) -> (Vec3 -> Vec3)
    gradient sf x = vec3 (partialX, partialY, partialZ)
      where
        partialX = (sf (vec3((x&x_) + epsilon, x&y_, x&z_)) - sf x) ^/ epsilon
        partialY = (sf (vec3(x&x_, (x&y_) + epsilon, x&z_)) - sf x) ^/ epsilon
        partialZ = (sf (vec3(x&x_, x&y_, (x&z_) + epsilon)) - sf x) ^/ epsilon
        epsilon = 0.00001



    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps

          shade sdf p = lightColor ^* (normal <.> lightDir)
            where
              normal = gradient sdf p
              lightColor = vec3(sin. (*i') . (*10) $(len uvN), 0.3, 0.9)
              lightPosition = vec3 (0, 1, 0)
              lightDir = normalize (lightPosition - p)

          newColor = (mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9))
            -- * shade sdf p

          cond = (d `lt` 0.001)
      in ( sel continue
           (sel cond
             newColor
             color
           )
           color
         , sel continue
           (sel cond
             t
             t + d
           )
           t
         , sel continue
           (sel cond
             false
             true
           )
           continue
         )


output :: Program
output = toProgram raymarch7
