module Asteroids where

import Graphics.Gloss
import DataTypes

-- asteroids in seperate file bec many modes will depend on 
-- and it will have constant behavior and may be not its up to khaled to decide
-- import this file whenever u want to call one of its functions
-- LASTLY NEVER EVER DELETE COMMENTS
--  u can delete some code u r free to  do it  :D

updateAsteroid :: asteroid -> asteroid --khaled edit this to fit the req
updateAsteroid asteroid = asteroid

renderAsteroid :: AsteroidsGame -> Picture
renderAsteroid game = pictures
  [
    mkAst (radius asteroid) (aLocation asteroid) | asteroid <- (asteroids game)
  ]
  where
    mkAst :: Float -> (Float, Float) -> Picture
    mkAst r (x,y) = pictures
     [
       scale 1 (0.8) (translate x y $ color (greyN 0.2) (circleSolid r))
     ]