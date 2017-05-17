module Asteroids where

import Graphics.Gloss
import DataTypes
import System.Random
import System.IO.Unsafe
import  Player
-- asteroids in seperate file bec many modes will depend on 
-- and it will have constant behavior and may be not its up to khaled to decide
-- import this file whenever u want to call one of its functions
-- LASTLY NEVER EVER DELETE COMMENTS
--  u can delete some code u r free to  do it  :D
-- |     traversing asteroid

updateAsteroid :: Asteroid -- ^  Asteroid need update  
 -> AsteroidsGame -- ^ current Game
 -> Asteroid   -- ^ return astroid after   update
updateAsteroid asteroid  game = asteroid { aLocation = newLocation (aLocation asteroid)}
                                 where newLocation (x,y) = (verifyXLocation game (x + (fst (aSpeed asteroid))),verifyYLocation game (y +(snd (aSpeed asteroid))))
-- |    return  random  value  with rang  x -x                              
rand:: Float->Float
rand x = unsafePerformIO (getStdRandom (randomR (-x, x)))



-- |  render Asteroid
renderAsteroid :: AsteroidsGame  -- ^  current game
  -> Picture -- ^ new  picture
renderAsteroid game = pictures
  [
    mkAst (radius asteroid) (aLocation asteroid) | asteroid <- (asteroids game)
  ]
  where
    mkAst :: Float -> (Float, Float) -> Picture
    mkAst r (x,y) = pictures
     [
       (translate x y $ color (greyN 0.2) (circleSolid r))
     ]

-- | check if  Asteriod died  initialize Asteroid again 
updateAsteroidList :: [Asteroid] -- ^ current  Asteroid 
 ->[Asteroid] -- ^ update Asteroid
updateAsteroidList [ ] =initializeAsteroids asteroidNo
updateAsteroidList asteroids =asteroids
-- | make  Asteroid iinitialize
initializeAsteroids  :: Float -- ^ Asteroid count
 -> [Asteroid] -- ^ Asteroid after  initialize 
initializeAsteroids 0 = []
initializeAsteroids count =   Asteroid                  -- idk how this worked but it did :D 
    { size = 2
    , aLocation = ( rand (awidth-count), rand (aheight - count ) )-- rand
    , aSpeed = (rand asteroidMaxSpeed , rand asteroidMaxSpeed)  -- rand
    , radius = 80 
    }  : initializeAsteroids (count - 1) 