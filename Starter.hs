
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Starter where

import Util

output = toProgram $ vec4 (v, v, v, 1)

v = vqF vq uvN

vqF x = x

vq uv = 0
