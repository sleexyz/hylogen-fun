{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

module SDFDemo where

import Hylogen.WithHylide

output :: Program
output = toProgram raymarch8


rot :: Vec1 -> Vec2 -> Vec2
rot phi a = vec2 ( cos phi * x_ a
                   + sin phi * y_ a
                 , (-1) * sin phi * x_ a
                   + cos phi * y_ a
                 )

rep :: forall n. Veccable n => Vec n ->  Vec n -> Vec n
rep c p = mod_  p c - 0.5 * c

box :: Vec3 -> Vec3 -> Vec3 -> Vec1
box boxPos dim p = len (max_ (abs (p - boxPos) - dim) 0)


raymarch8 :: Vec4
raymarch8 = id
  . (\x -> vec4(x, 1))
  . (\(x, _, _) -> x)
  $ foldr fn  (black, 0, true) (fromInteger <$> [1..maxSteps])
  where

    eye = vec3 (0, 0, -1)
    up = vec3 (0, 1, 0)
    right = vec3 (1, 0, 0)
    maxSteps = 32


    ro = vec3 (rot time uvN, tan (time * (-0.1)))
    rd = eye ^* 0.8 + right ^* x_ uvN + up ^* y_ uvN --perspective!
      & (\x -> vec3 (rot (time * 0.1) (vec2(x_ x, y_ x)), z_ x))
      & normalize


    sdf :: Vec3 -> Vec1
    sdf p = (box (vec3(mouse, -2.5)) 0.4 (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(5,0.1, 0.1)) (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(0.1,5, 0.1)) (f p))
            `min_` (box (vec3(mouse, -2.5)) (vec3(0.1,0.1, 5)) (f p))
      where
        f = rep 5


    fn :: Vec1 -> (Vec3, Vec1, Booly) -> (Vec3, Vec1, Booly)
    fn i (color, t, continue) =
      let p = ro + rd ^* t
          d = sdf p

          i' = i/fromInteger maxSteps
          newColor = (mix i' (vec3(0.1, 0, 0.2)) (white ^* 0.9))

          cond = (d `lt` 0.001)
      in ( sel continue (sel cond newColor color) color
         , sel continue (sel cond t (t + d)) t
         , sel continue (sel cond false true) continue
         )


