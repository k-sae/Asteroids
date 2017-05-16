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
-- updateSinglePlayerGame :: AsteroidsGame -> AsteroidsGame 
-- updateSinglePlayerGame  = projectilesCollision . updatePlayerAsteroidCollisionV2 . updateGamePlayersStates 



--collisions
updatePlayerAsteroidCollisionV2 ::AsteroidsGame ->AsteroidsGame
updatePlayerAsteroidCollisionV2 game = game {asteroids = getAsteroids getItems, players = newPlayers (players game)}
                                        where getItems = [ getCollisionItem player asteroid | player <- (players game) , asteroid <- (asteroids game)]
                                              getCollisionItem player asteroid = CollisionItem {cPlayers = player, cAsteroid = asteroid}
                                              newPlayers players | getupdatedPlayers == [] = players
                                                                 | otherwise = getupdatedPlayers
                                              getupdatedPlayers = getPlayers getItems
getAsteroids :: [CollisionItem] ->[Asteroid] 
getAsteroids [] = []
getAsteroids (i:cIs) | distance (cAsteroid i) (cPlayers i) > (radius (cAsteroid i)) = (cAsteroid i)  : getAsteroids cIs
                     | otherwise =  (breakeAsteroid (cAsteroid i)  2) ++ getAsteroids cIs
               where distance ast player = sqrt (( fst (plLocation player) - fst (aLocation ast))^2 + ( snd (plLocation player) - snd (aLocation ast))^2)

--DONT USE THIS IN MULTI 
--Fix This later will introduce a bug in multi player
getPlayers:: [CollisionItem] ->[Player] 
getPlayers [] = []
getPlayers (p:pIs) |  distance (cAsteroid p) (cPlayers p) < (radius (cAsteroid p)) = (cPlayers p) {lives = (lives (cPlayers p)) - 1, plLocation = (0,0), plSpeed = (0,0) } : getPlayers pIs
                   
                   | otherwise = getPlayers pIs
               where distance ast player = sqrt (( fst (plLocation player) - fst (aLocation ast))^2 + ( snd (plLocation player) - snd (aLocation ast))^2)



breakeAsteroid :: Asteroid ->Float->[Asteroid] 
breakeAsteroid  asteroid count 
  |count == 0 ||  (size asteroid)  == 0 =[ ]
breakeAsteroid asteroid  count=asteroid{
  size = (size asteroid) -1
  , aLocation = ( fst (aLocation asteroid), snd (aLocation asteroid) )
  , aSpeed = (randX count, randY count) 
  , radius = (radius asteroid) / 2
} :  breakeAsteroid asteroid  (count -1)

--funcation not  use  but  kareem need
updateCollisions :: AsteroidsGame -> AsteroidsGame 
updateCollisions game =  game { players = [updatePlayerAsteroidCollision game player (asteroids game) |player <- (players game)]}





updatePlayerAsteroidCollision :: AsteroidsGame -> Player -> [Asteroid] -> Player
updatePlayerAsteroidCollision game player asteroids | length asteroids == length newAsteroids = player
                                               | otherwise = player {lives = (lives player) - 1, plLocation = (0,0), plSpeed = (0,0) }
                                  where distance ast = sqrt (( fst (plLocation player) - fst (aLocation ast))^2 + ( snd (plLocation player) - snd (aLocation ast))^2)
                                        newAsteroids = [ asteroid | asteroid <- asteroids, distance asteroid > (radius asteroid)]


----------Game Updates
updateSinglePlayerGame ::  AsteroidsGame -> AsteroidsGame 
updateSinglePlayerGame  = updateGamePlayersStates . initializeOnePlayer

initializeOnePlayer :: AsteroidsGame -> AsteroidsGame
initializeOnePlayer game | length(players game) == 0 = game{players = initializePlayers 1}
                     | otherwise = game

-- 'Function Composition'
updateGamePlayersStates :: AsteroidsGame -> AsteroidsGame 
updateGamePlayersStates game  = game {players = updatePlayers game
                                     ,asteroids = [updateAsteroid asteroid game| asteroid <- updateAsteroidList(asteroids game)] } 

--------Events Hndling
handleSingleplayerKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleSingleplayerKeys _ game = game

spRender :: AsteroidsGame -> Picture
spRender game = pictures
   ([
      mkShip (isThrusting player) (plColor player) (plLocation player) $ (degree player) | player <- (players game) -- Belal Check This  <-- :)
   ]
   ++
   [
      mkFire (projectiles player) | player <- (players game)
   ]
   ++
   [
      mkTitles (highScore player) (score player) (lives player) (plColor player) | player <- (players game)
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

    mkFire :: [Projectile] -> Picture
    mkFire projectiles = pictures [translate (fst (prLocation projectile)) (snd (prLocation projectile)) (color red (circleSolid 5)) | projectile <- projectiles]

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