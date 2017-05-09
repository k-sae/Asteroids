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
                                 where newLocation (x,y) = (verifyXLocation game (x + (fst (aSpeed asteroid))),verifyYLocation game (y +(snd (aSpeed asteroid))))
                                    
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
randX :: Float->Float
randX x = unsafePerformIO (getStdRandom (randomR (-x, x)))

randY :: Float ->Float
randY y = unsafePerformIO (getStdRandom (randomR (-y, y)))


renderAsteroid :: AsteroidsGame -> Picture
renderAsteroid game = pictures
  [
    mkAst (radius asteroid) (aLocation asteroid) | asteroid <- (asteroids game)
  ]
  where
    mkAst :: Float -> (Float, Float) -> Picture
    mkAst r (x,y) = pictures
     [
       scale 1 (0.8) (translate x y $ color (greyN 0.95) (circle r))
       --scale 1 (0.8) (translate (x-r/3) (y) $ color (greyN 0.3) (circleSolid (r/3))),
       --scale 1 (0.8) (translate (x) (y-r/2) $ color (greyN 0.3) (circleSolid (r/3)))
       --translate x y $ color (greyN 0.8) (polygon [((-70),(-70)),((-80),30),((-40),80),(10,50),(50,60),(80,(-10)),(50,30),(80,(-70)),((-70),(-70))])
     ]



initializeAsteroids  :: Float-> [Asteroid]
initializeAsteroids 0 = []
initializeAsteroids count =   Asteroid                  -- idk how this worked but it did :D 
    { size = 10
    , aLocation = ( randX (awidth-count), randY (aheight - count ) )-- rand
    , aSpeed = (randX count, randY count)  -- rand
    , radius = 80
    }  : initializeAsteroids (count - 1) 