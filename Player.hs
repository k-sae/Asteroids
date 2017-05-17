module Player where
import DataTypes
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game


initializePlayers :: Float -> [Player]
initializePlayers 1 = [Player                  -- idk how this worked but it did :D 
    { pID = 1
    , projectiles = []
    , degree      = 0
    , plSpeed     = (0,0)
    , plLocation  = (0,0)
    , rotatingBy  = rotationSpeed
    , firingSpeed = 10
    , isrotating  = False
    , isFiring    = False
    , firemode    = 1
    , plColor     = (makeColorI 51 122 183 255)
    , isThrusting = False
    , score       = 0
    , highScore   = 0
    , lives       = 3
    }]
    
initializePlayers 2 = [Player                  -- idk how this worked but it did :D 
    { pID = 1
    , projectiles = []
    , degree      = 180
    , plSpeed     = (0,0)
    , plLocation  = ((-200),0)
    , rotatingBy  = rotationSpeed
    , firingSpeed = 10
    , isrotating  = False
    , isFiring    = False
    , firemode    = 1
    , plColor     = (makeColorI 51 122 183 255)
    , isThrusting = False
    , score       = 0
    , highScore   = 0
    , lives       = 3
    },
    Player                  -- idk how this worked but it did :D 
    { pID = 2
    , projectiles = []
    , degree      = 0
    , plSpeed     = (0,0)
    , plLocation  = (200,0)
    , rotatingBy  = rotationSpeed
    , firingSpeed = 10
    , isrotating  = False
    , isFiring    = False
    , firemode    = 1
    , plColor     = (makeColorI 51 222 10 255)
    , isThrusting = False
    , score       = 0
    , highScore   = 0
    , lives       = 3
    }]

--The x value will be the rotatingBy value!
updateRotationStates :: Float -> Bool -> [Player] -> Int -> [Player] 
updateRotationStates x rotationState players index = updateRotationStatesHelper index 0 players rotationState x

--handle player interaction according to its index
updateRotationStatesHelper ::  Int -> Int -> [Player] -> Bool -> Float-> [Player]
updateRotationStatesHelper _ _ [] _ _ = [] 
updateRotationStatesHelper playerIndex startCount (p:players) rotationState x
                                                                           | startCount == playerIndex = p {isrotating = rotationState , rotatingBy = x}:updateRotationStatesHelper playerIndex (startCount+1) players rotationState x
                                                                           | otherwise = p:updateRotationStatesHelper playerIndex (startCount+1) players rotationState x
updateThrustStatus :: [Player] -> Bool -> Int -> Int -> [Player]
updateThrustStatus [] _ _ _ = []
updateThrustStatus (p:players) state index startIndex 
                                                   | startIndex == index = p { isThrusting = state} : updateThrustStatus players state index (startIndex+1) 
                                                   | otherwise = p : updateThrustStatus players state index (startIndex+1) 

updateFireStatus :: [Player] -> Bool -> Int -> Int ->[Player]
updateFireStatus [] _ _ _ = []
updateFireStatus (p:players) status index startIndex 
                                                   |startIndex == index = p { isFiring = status} : updateFireStatus players status index (startIndex+1) 
                                                   |otherwise = p : updateFireStatus players status index (startIndex+1) 


updatePlayers :: AsteroidsGame -> [Player] -- sry for doing this but its working :) 
updatePlayers game = [(updateProjectiles game .updateSpeed.rotateBy.updateLocationBy game) x|x <- (players game)]

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
                   where a = (gWidth game)

verifyYLocation :: AsteroidsGame -> Float -> Float
verifyYLocation game x 
                 | abs x >= a/2= -x
                 | otherwise = x
                   where a = (gHeight game)



updateSpeed :: Player -> Player
updateSpeed player | isThrusting player == True = player{plSpeed = newSpeed (plSpeed player)}
                   | otherwise = player -- EPIC Equation
            where newSpeed (x,y) = (check (x + (cos (degToRad ((degree player) - 180))) * accelerateSpeed),check (y + (sin (degToRad ((degree player)-180))) * accelerateSpeed))
                  check x  | x > thrustMaxSpeed = thrustMaxSpeed
                           | x < -thrustMaxSpeed = -thrustMaxSpeed
                           |otherwise  = x



-- u may not need to touch this 
updateProjectiles :: AsteroidsGame ->Player-> Player
updateProjectiles game  player= player { projectiles = updateProjectilesCount [updateProjectile game projectile player| projectile <- (projectiles player),(prLifeTime projectile) > 0] player,
                                   firingSpeed = (firingSpeed player) + 1}

updateProjectilesCount :: [Projectile] -> Player -> [Projectile]
updateProjectilesCount projectiles player 
                                         | (isFiring player) == False = projectiles
                                         | (firingSpeed player) `mod` 15 ==0 = initializeProjectile player : projectiles
                                         | otherwise = projectiles

initializeProjectile :: Player -> Projectile
initializeProjectile player = Projectile
                              {
                                 prLocation = (plLocation player)
                                ,prSpeed = ((cos (degToRad ((degree player) - 180))) * 3,(sin (degToRad ((degree player) - 180))) * 3)
                                ,prLifeTime = 1000
                              }

--update projectile Hazem will have Fun here 
updateProjectile :: AsteroidsGame->  Projectile -> Player -> Projectile
updateProjectile  game projectile player = projectile {  prLocation = newProjectileLocation
                                                 , prLifeTime = prLifeTime projectile - 10 
                                                }
                                                where newProjectileLocation =  (verifyXLocation game (fst (prLocation projectile) + (fst (prSpeed projectile) *5)  ), ( verifyYLocation game (snd (prLocation projectile) + (snd (prSpeed projectile) *5))))
--TODO
--    1- initialize upon key event
--    2-