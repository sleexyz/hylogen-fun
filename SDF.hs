{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

module SDF where

import Hylogen.WithHylide
import Data.VectorSpace
import Data.Function
import Control.Arrow

-- Signed distance fields
type DistFunc  = Vec3 -> Vec1


infixl 5 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

sphere :: Vec3 -> Vec1 -> Vec3 -> Vec1
sphere spherePos radius eyePos = len (eyePos - spherePos) - radius

sel = select
black = vec3 (0, 0, 0)
white = vec3 (1, 1, 1)

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

    fn :: Vec1 -> (Vec3, Vec1) -> (Vec3, Vec1)
    fn i (color, t) =
      let p = ro + rd ^* t
          d = sphere (vec3 (0, 0, 0)) 0.5 p
          newColor = vec3(1, 1, 1)
      in ( select (d `lt` 0.000001) newColor color
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
    eye = vec3 (0, 0, -1)
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
    rd = eye ^* (0.8) + right ^* uvN!X + up ^* uvN!Y --perspective!


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
    rd = eye ^* (0.9) + right ^* uvN!X + up ^* uvN!Y --perspective!


    maxSteps :: Integer
    maxSteps = 64

    plane :: Vec3 -> Vec3 -> Vec3 -> Vec1
    plane planePos normal p = normal <.> (p - planePos)

    repeat :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
    repeat c p = mod_  p c - 0.5 * c

    sdf :: Vec3 -> Vec1
    sdf p = sphere (vec3(mouse, -2)) (1) (repeat 3 p)
            `min_` plane (vec3(-1, -1, -1)) (vec3 (0, 1, 0)) p
            -- `min_` plane (vec3(-1, 1, -1)) (vec3 (0, -1, 0)) p
            -- `min_` plane (vec3(-1, -1, -1)) (vec3 (1, 0, 0)) p
            -- `min_` plane (vec3(1, -1, -1)) (vec3 (-1, 0, 0)) p




    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = mix i' (vec3(0.1, 0, 0.2)) (white)

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


main = putStrLn . toGLSL $ raymarch3
