module Asteroids where

import Graphics.Gloss
import DataTypes
import System.Random
import System.IO.Unsafe
-- asteroids in seperate file bec many modes will depend on 
-- and it will have constant behavior and may be not its up to khaled to decide
-- import this file whenever u want to call one of its functions
-- LASTLY NEVER EVER DELETE COMMENTS
--  u can delete some code u r free to  do it  :D

updateAsteroid :: Asteroid -> AsteroidsGame-> Asteroid --khaled edit this to fit the req
updateAsteroid asteroid  game = asteroid { aLocation = newLocation (aLocation asteroid)}
                                 where newLocation (x,y) = (verifyXLocation game (x +randX),verifyYLocation game (y +randY))
                                    
verifyXLocation :: AsteroidsGame -> Float -> Float
verifyXLocation game x 
                 | abs x >= a/2= -x
                 | otherwise = x
                   where a = (gWidth game)

verifyYLocation :: AsteroidsGame -> Float -> Float
verifyYLocation game x 
                 | abs x >= a/2= -x
                 | otherwise = x
                   where a = (gHeight game)
randX :: Float
randX  = unsafePerformIO (getStdRandom (randomR (-2, 2)))

randY :: Float
randY = unsafePerformIO (getStdRandom (randomR (-2, 2)))


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