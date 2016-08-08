-- {-# LANGUAGE 
module New where
import Hylogen.WithHylide

output = toProgram $ vec4 (r v, g v, b v, 0.01) + bb
 where
   (r, g, b) = (id, id, id)
   v = sin (len uvN + time)  + cos (atan(x_ uvN / (y_ uvN) ** 2 * 10))
   bb = texture2D backBuffer $ ((uvN * 1.2) * 0.5 + 0.5)
   -- v = cos (atan ((i^*)$ y_ uvN) * (1 - x_ audio) + sin time) **2
   --   + sin (atan ((i^*) $ y_ uvN) * (1 - x_ audio) + cos time) **2
   --   where
   --     (i, j) = (3, 3)
   -- v = cos $ sum [ atan (y_ uvN + cos (x_ uvN)) * 100
   --               , cos (y_ uvN + time/4) * 100
   --               , sin (x_ uvN + time/4) * 100
   --               , time
   --               ]
