{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intro where

import Hylogen.WithHylide

output :: Program
output = toProgram color

color   = vec4(1, 1, 1, 1)


-- Equivalent expressions

color'  = vec4(white, 1)   -- white == vec3(1, 1, 1)

color'' = 1                -- the type of 1 is inferred to be a vec4
                           -- which is equivalent to vec4 (1.0, 1.0, 1.0, 1.0)


