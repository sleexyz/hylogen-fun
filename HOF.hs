{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Higher order functions!

module HOF where

import Hylogen.WithHylide


output :: Program
output = toProgram $ vec4 (color, 1)


color :: vec3
color = copy a
  where
    



