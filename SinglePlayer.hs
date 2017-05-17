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
-- | Update the single player mode
updateSinglePlayerGame ::  AsteroidsGame -> AsteroidsGame 
updateSinglePlayerGame  = updateGamePlayersStates . initializeOnePlayer

-- | Initialize the game by one player for the single player mode
initializeOnePlayer :: AsteroidsGame -> AsteroidsGame
initializeOnePlayer game | length(players game) == 0 = game{players = initializePlayers 1}
                     | otherwise = game

-- 'Function Composition'
-- | Update the players and the asteroids
updateGamePlayersStates :: AsteroidsGame -> AsteroidsGame 
updateGamePlayersStates game  = game {players = updatePlayers game
                                     ,asteroids = [updateAsteroid asteroid game| asteroid <- updateAsteroidList(asteroids game)] } 

--------Events Hndling
-- | Handle the player 1 keys.
handleSingleplayerKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleSingleplayerKeys _ game = game

-- | Display the basic contents of the single player mode like the player1 ship, fire, the titles, etc
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
    -- | Display the player's ship
    mkShip 
     :: Bool -- ^ Indicate whether the ship is thrusting or not
     -> Color -- ^ The ship color
     -> (Float, Float) -- ^ The ship location
     -> Float -- ^ The ship rotaion 
     -> Picture
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

    -- | Display the ship fire
    mkFire :: [Projectile] -> Picture
    mkFire projectiles = pictures [translate (fst (prLocation projectile)) (snd (prLocation projectile)) (color red (circleSolid 5)) | projectile <- projectiles]

    -- | Display some titles like player's score, lives, etc
    mkTitles :: Float -> Float -> Float -> Color -> Picture
    mkTitles hs s l col = pictures
     [
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (gHeight game*2.2) $ color white (text ("High Score: " ++ show (hs)))),
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (gHeight game*2) $ color white (text ("Score: " ++ show (s)))),
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (-(gHeight game*2.2)) $ color white (text "Lives: " )),
      showLives l col
     ]

    -- | Display the number of player's lives
    showLives :: Float -> Color -> Picture
    showLives n col = pictures [scale (0.8) (0.8) (mkShip False col ((-gWidth game)*(0.57) + 70 + x*40,(-gHeight game)*(0.55) + 32) 270) | x <- [1..n] ]