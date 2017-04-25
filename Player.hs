module Player where
import DataTypes
import Graphics.Gloss.Geometry.Angle

updatePlayers :: AsteroidsGame -> [Player] -- sry for doing this but its working :) 
updatePlayers game = [(updateProjectiles.updateSpeed.rotateBy.updateLocationBy game) x|x <- (players game)]

rotateBy :: Player -> Player 
rotateBy player |(isrotating player) == True =  player {degree = newdegree}
                | otherwise = player
 where newdegree = (rotatingBy player) + (degree player)

updateLocationBy :: AsteroidsGame -> Player -> Player
updateLocationBy game player = player {plLocation = newLocation (plLocation player)}
                       where newLocation (x,y) = (verifyXLocation game (x + xvelocity (plSpeed player)),verifyYLocation game (y + yvelocity (plSpeed player)))
                             xvelocity (x,_) = x
                             yvelocity (_,y) = y
verifyXLocation :: AsteroidsGame -> Float -> Float
verifyXLocation game x 
                 | abs x >= a/2= -x
                 | otherwise = x
                   where a = fromIntegral (gWidth game) :: Float

verifyYLocation :: AsteroidsGame -> Float -> Float
verifyYLocation game x 
                 | abs x >= a/2= -x
                 | otherwise = x
                   where a = fromIntegral (gHeight game) :: Float



updateSpeed :: Player -> Player
updateSpeed player | isThrusting player == True = player{plSpeed = newSpeed (plSpeed player)}
                   | otherwise = player -- EPIC Equation
            where newSpeed (x,y) = (check (x + (cos (degToRad ((degree player) - 180))) * accelerateSpeed),check (y + (sin (degToRad ((degree player)-180))) * accelerateSpeed))
                  check x  | x > thrustMaxSpeed = thrustMaxSpeed
                           | x < -thrustMaxSpeed = -thrustMaxSpeed
                           |otherwise  = x

-- u may not need to touch this 
updateProjectiles :: Player -> Player
updateProjectiles player = player { projectiles = [updateProjectile projectile| projectile <- (projectiles player)]}



--update projectile Hazem will have Fun here 
updateProjectile :: Projectile -> Projectile
updateProjectile projectile = projectile