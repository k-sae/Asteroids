module SinglePlayer where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Shapes
import Asteroids
import Player
import System.Random
import Debug.Trace
----------Game Updates
updateSinglePlayerGame :: AsteroidsGame -> AsteroidsGame 
updateSinglePlayerGame  = updatePlayerAsteroidCollisionV2 . updateGamePlayersStates 

data CollisionItem = CollisionItem 
     { cPlayers :: Player,
       cAsteroid :: Asteroid
     }

--collisions
updateCollisions :: AsteroidsGame -> AsteroidsGame 
updateCollisions game =  game { players = [updatePlayerAsteroidCollision game player (asteroids game) |player <- (players game)]}


updatePlayerAsteroidCollisionV2 game = game {asteroids = getAsteroids getItems, players = newPlayers (players game)}
                                        where getItems = [ getCollisionItem player asteroid | player <- (players game) , asteroid <- (asteroids game)]
                                              getCollisionItem player asteroid = CollisionItem {cPlayers = player, cAsteroid = asteroid}
                                              newPlayers players | getupdatedPlayers == [] = players
                                                                 | otherwise = getupdatedPlayers
                                              getupdatedPlayers = getPlayers getItems
getAsteroids [] = []
getAsteroids (i:cIs) | distance (cAsteroid i) (cPlayers i) > (radius (cAsteroid i)) = (cAsteroid i)  : getAsteroids cIs
                     | otherwise = getAsteroids cIs
               where distance ast player = sqrt (( fst (plLocation player) - fst (aLocation ast))^2 + ( snd (plLocation player) - snd (aLocation ast))^2)

--DONT USE THIS IN MULTI 
--Fix This later will introduce a bug in multi player
getPlayers [] = []
getPlayers (p:pIs) |  distance (cAsteroid p) (cPlayers p) < (radius (cAsteroid p)) = (cPlayers p) {lives = (lives (cPlayers p)) - 1, plLocation = (0,0), plSpeed = (0,0) } : getPlayers pIs
                   | otherwise = getPlayers pIs
               where distance ast player = sqrt (( fst (plLocation player) - fst (aLocation ast))^2 + ( snd (plLocation player) - snd (aLocation ast))^2)


updatePlayerAsteroidCollision :: AsteroidsGame -> Player -> [Asteroid] -> Player
updatePlayerAsteroidCollision game player asteroids | length asteroids == length newAsteroids = player
                                               | otherwise = player {lives = (lives player) - 1, plLocation = (0,0), plSpeed = (0,0) }
                                  where distance ast = sqrt (( fst (plLocation player) - fst (aLocation ast))^2 + ( snd (plLocation player) - snd (aLocation ast))^2)
                                        newAsteroids = [ asteroid | asteroid <- asteroids, distance asteroid > (radius asteroid)]
-- 'Function Composition'
updateGamePlayersStates :: AsteroidsGame -> AsteroidsGame 
updateGamePlayersStates game  = game {players = updatePlayers game
                                     ,asteroids = [updateAsteroid asteroid game| asteroid <- updateAsteroidList(asteroids game)] } 

--------Events Hndling
handleSingleplayerKeys (EventKey (Char 'd') Down _ _) game = game { players = updateRotationStates (-rotationSpeed) True (players game) 0}    -- Rotate the ship Clock-Wise when press 'd'
handleSingleplayerKeys (EventKey (Char 'd') Up _ _) game = game { players = updateRotationStates (-rotationSpeed) False (players game) 0}

handleSingleplayerKeys (EventKey (Char 'a') Down _ _) game = game { players = updateRotationStates (rotationSpeed) True (players game) 0}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleSingleplayerKeys (EventKey (Char 'a') Up _ _) game = game { players = updateRotationStates (rotationSpeed) False (players game) 0}
handleSingleplayerKeys (EventKey (Char 'p') Down _ _) game = game {gameMode = Pause}   -- Pause the game when press 'p'
handleSingleplayerKeys (EventKey (Char 'q') Down _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'
handleSingleplayerKeys (EventKey (Char 'w') Down _ _) game = game {players = updateThrustStatus (players game) True 0 0}
handleSingleplayerKeys (EventKey (Char 'w') Up _ _) game = game {players = updateThrustStatus (players game) False 0 0}
handleSingleplayerKeys (EventResize (w,h)) game = game {gWidth = (fromIntegral w) , gHeight = (fromIntegral h)}
handleSingleplayerKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game {players = updateFireStatus (players game) True}
handleSingleplayerKeys (EventKey (SpecialKey KeySpace) Up _ _) game = game {players = updateFireStatus (players game) False}

handleSingleplayerKeys _ game = game

--hazem add key event on spacebar to fire 
-- u may use this reference: https://hackage.haskell.org/package/gloss-1.11.1.1/docs/Graphics-Gloss-Interface-IO-Game.html


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
                                                   | startIndex == index = p { isThrusting = state} : updateThrustStatus players state index startIndex 
                                                   | otherwise = p : updateThrustStatus players state index startIndex 

updateFireStatus :: [Player] -> Bool -> [Player]
updateFireStatus players status = [player{isFiring = status} | player <- players]

spRender :: AsteroidsGame -> Picture
spRender game = pictures
   ([
      --assume this value is the number of stars
      mkStars 111
   ]
   ++
   [
      mkTitles (highScore player) (score player) (lives player) (plColor player) | player <- (players game)
   ]
   ++
   [
      renderAsteroid game
   ]
   ++
   [
      mkShip (isThrusting player) (plColor player) (plLocation player) $ (degree player) | player <- (players game) -- Belal Check This  <-- :)
   ]
   ++
   [
      mkFire (projectiles player) | player <- (players game)
   ])
   
   where
    mkShip :: Bool -> Color -> (Float, Float) -> Float -> Picture
    mkShip False col (x,y) degree = pictures
     [
       translate x y $ color white $ solidArc (degree-20) (degree+20) 40,
       translate x y $ color col $ solidArc (degree-15) (degree+15) 37
     ]
    mkShip True col (x,y) degree = pictures
     [
       translate x y $ color red $ solidArc (degree-5) (degree+5) 47,
       translate x y $ color white $ solidArc (degree-20) (degree+20) 40,
       translate x y $ color col $ solidArc (degree-15) (degree+15) 37
     ]

    mkStars :: Int -> Picture
    mkStars n = pictures
     [
       translate (fst l) (snd l) $ color blue (circleSolid 2) | l <- getVal (randX n) (randY n)
     ]

    mkFire :: [Projectile] -> Picture
    mkFire projectiles = pictures [translate (fst (prLocation projectile)) (snd (prLocation projectile)) (color red (circleSolid 5)) | projectile <- projectiles]

    randX :: Int -> [Float]
    randX n = take n (randomRs ((-(gWidth game)), (gWidth game) :: Float) (mkStdGen n))
    randY :: Int -> [Float]
    randY n = take n (randomRs ((-(gHeight game)), (gHeight game) :: Float) (mkStdGen (n*2)))

    getVal :: [Float] -> [Float] -> [(Float,Float)] 
    getVal [] [] = []
    getVal [] _ = []
    getVal _ [] = []
    getVal (x:xs) (y:ys) = (x,y) : (getVal xs ys)

    mkTitles :: Float -> Float -> Float -> Color -> Picture
    mkTitles hs s l col = pictures
     [
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (gHeight game*2.2) $ color white (text ("High Score: " ++ show (hs)))),
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (gHeight game*2) $ color white (text ("Score: " ++ show (s)))),
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (-(gHeight game*2.2)) $ color white (text "Lives: " )),
      showLives l col
     ]

    showLives :: Float -> Color -> Picture
    showLives n col = pictures [scale (0.8) (0.8) (mkShip False col ((-gWidth game)*(0.57) + 70 + x*40,(-gHeight game)*(0.55) + 32) 270) | x <- [1..n] ]