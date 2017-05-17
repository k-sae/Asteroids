module Shapes where
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

-- | Draw a ship between a given 2 degrees and radius 
solidArc 
 :: Float -- ^ The first degree
 -> Float -- ^ The second degree
 -> Float -- ^ The radius degree
 -> Picture
solidArc a1 a2 r = Pictures 
        [ 
          Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))],
          Pictures ([ Line [(0, 0), (r * cos (degToRad x), r * sin (degToRad x))]| x <- [a1..a2]]),
          Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))]
        ]