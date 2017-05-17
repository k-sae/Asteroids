module Player where
import DataTypes
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game


initializePlayers :: Float  -- ^ number  of player 
 -> [Player]  -- ^ return new list of player
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
--    , plColor     = (makeColorI 51 122 183 255)
    , plColor = orange
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
    , plColor     = orange
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
-- | update Rotation status
updateRotationStates :: [Player] -- ^ list of players
 ->Float-- ^ rotation speed
 -> Bool -- ^ status
 -> Int -- ^ playerIndex
 -> [Player] -- ^ list of players with updated player
updateRotationStates [] _ _ _  = []
updateRotationStates (p:players) x state  index  
                                          | (pID p) == index = p { isrotating = state,rotatingBy= x} : updateRotationStates players x state index 
                                          | otherwise = p : updateRotationStates players x state  index 

-- | update thrust status
updateThrustStatus :: [Player] -- ^ list of players
 -> Bool -- ^ status
 -> Int -- ^ playerIndex
 -> [Player] -- ^ list of players with updated player
updateThrustStatus [] _ _  = []
updateThrustStatus (p:players) state index 
                                          | (pID p) == index = p { isThrusting = state} : updateThrustStatus players state index
                                          | otherwise = p : updateThrustStatus players state index

-- | update firestatus for player 
updateFireStatus :: [Player] -- ^ list of players
 -> Bool -- ^ status
 -> Int -- ^ playerindex
 ->[Player] -- ^  list of players with updated player
updateFireStatus [] _ _  = []
updateFireStatus (p:players) status index 
                                          |(pID p) == index = p { isFiring = status} : updateFireStatus players status index
                                          |otherwise = p : updateFireStatus players status index

-- sry for doing this but its working :)
-- | update player projectiles && speed && rrotation && speed  && location
updatePlayers :: AsteroidsGame -- ^ current game 
 -> [Player] -- ^ updated list of players                                            
updatePlayers game = [(updateProjectiles game .updateSpeed.rotateBy.updateLocationBy game) x|x <- (players game)]

-- | update degree of player during rotatation
rotateBy :: Player -- ^ current player
 -> Player -- ^ updated  player with new degree
rotateBy player |(isrotating player) == True =  player {degree = newdegree}
                | otherwise = player
 where newdegree = (rotatingBy player) + (degree player)

-- | update player location  in x and  y
updateLocationBy :: AsteroidsGame -- ^ current game 
 -> Player -- ^ current player 
 -> Player -- ^ updated player location
updateLocationBy game player = player {plLocation = newLocation (plLocation player)}
                       where newLocation (x,y) = (verifyXLocation game (x + xvelocity (plSpeed player)),verifyYLocation game (y + yvelocity (plSpeed player)))
                             xvelocity (x,_) = x
                             yvelocity (_,y) = y

-- |  check  if   location x  equal  max width  return -x  
verifyXLocation :: AsteroidsGame  -- ^current  game
 -> Float -- ^ current  x location 
 -> Float -- ^  new  x location
verifyXLocation game x 
                 | abs x >= a/2= -x
                 | otherwise = x
                   where a = (gWidth game)

-- |  check  if   location y  equal max hight  return -y
verifyYLocation :: AsteroidsGame  -- ^current  game
 -> Float  -- ^ current  y location 
 -> Float -- ^  new  y location
verifyYLocation game x 
                 | abs x >= a/2= -x
                 | otherwise = x
                   where a = (gHeight game)


-- | update player speed 
updateSpeed :: Player  -- ^ current player
 -> Player -- ^ current player with new speed
updateSpeed player | isThrusting player == True = player{plSpeed = newSpeed (plSpeed player)}
                   | otherwise = player -- EPIC Equation
            where newSpeed (x,y) = (check (x + (cos (degToRad ((degree player) - 180))) * accelerateSpeed),check (y + (sin (degToRad ((degree player)-180))) * accelerateSpeed))
                  check x  | x > thrustMaxSpeed = thrustMaxSpeed
                           | x < -thrustMaxSpeed = -thrustMaxSpeed
                           |otherwise  = x



-- | add prjoectiles to player 
updateProjectiles :: AsteroidsGame -- ^ current  game
 -> Player -- ^ current player
 -> Player -- ^  player with projectiles updated
updateProjectiles game  player= player { projectiles = updateProjectilesCount [updateProjectile game projectile player| projectile <- (projectiles player),(prLifeTime projectile) > 0] player,
                                   firingSpeed = (firingSpeed player) + 1}

-- | check projectile before adding to player
updateProjectilesCount :: [Projectile] -- ^ list of projectiles
 -> Player  -- ^ current player
 -> [Projectile] -- ^ list of projectiles 
updateProjectilesCount projectiles player 
                                         | (isFiring player) == False = projectiles
                                         | (firingSpeed player) `mod` fireDelay ==0 = initializeProjectile player : projectiles
                                         | otherwise = projectiles

-- | make a new projectile 
initializeProjectile :: Player -- ^  current player
 -> Projectile -- ^ inilized  projectile 
initializeProjectile player = Projectile
                              {
                                 prLocation = (plLocation player)
                                ,prSpeed = ((cos (degToRad ((degree player) - 180))) * thrustMaxSpeed * 0.5,(sin (degToRad ((degree player) - 180))) * thrustMaxSpeed * 0.5)
                                ,prLifeTime = projectileLife
                              }

-- | change location of projectile  and decrease  life time 
updateProjectile :: AsteroidsGame -- ^ current  game 
 ->  Projectile  -- ^ all projectile 
 -> Player       -- ^ current player
 -> Projectile   -- ^ updated  projectile
updateProjectile  game projectile player = projectile {  prLocation = newProjectileLocation
                                                 , prLifeTime = prLifeTime projectile - 10 
                                                }
                                                where newProjectileLocation =  (verifyXLocation game (fst (prLocation projectile) + (fst (prSpeed projectile) *5)  ), ( verifyYLocation game (snd (prLocation projectile) + (snd (prSpeed projectile) *5))))
