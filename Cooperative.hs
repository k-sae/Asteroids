module Cooperative where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Shapes
import Asteroids
import Player
import System.Random
----------Game Updates
-- | Update the cooperative mode
updateCooperativeGame :: AsteroidsGame -> AsteroidsGame 
updateCooperativeGame  = updateGamePlayersStates . initializeTwoPlayer

-- | Initialize the game by two player for the cooperative mode
initializeTwoPlayer :: AsteroidsGame -> AsteroidsGame
initializeTwoPlayer game | length(players game) < 1 = game{players = initializePlayers 2}
                         | otherwise = game

-- 'Function Composition'
-- | Update the players and the asteroids
updateGamePlayersStates :: AsteroidsGame -> AsteroidsGame 
updateGamePlayersStates game  = game {players = updatePlayers game
                                     ,asteroids = [updateAsteroid asteroid game| asteroid <- updateAsteroidList(asteroids game)] } 

--------Events Hndling
-- | Handle the player 2 keys, like thrust rotate, fire, etc
handleCooperativeKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleCooperativeKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { players = updateRotationStates  (players game) (-rotationSpeed) True 2}    -- Rotate the ship Clock-Wise when press 'd'
handleCooperativeKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { players = updateRotationStates  (players game) (-rotationSpeed) False 2}
handleCooperativeKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { players = updateRotationStates  (players game) (rotationSpeed) True 2}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleCooperativeKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { players = updateRotationStates  (players game) (rotationSpeed) False 2}
handleCooperativeKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {players = updateThrustStatus (players game) True 2}
handleCooperativeKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {players = updateThrustStatus (players game) False 2}
handleCooperativeKeys (EventKey  (Char 'k') Down _ _) game = game {players = updateFireStatus (players game) True 2}
handleCooperativeKeys (EventKey  (Char 'k') Up _ _) game = game {players = updateFireStatus (players game) False 2}
handleCooperativeKeys _ game = game

-- | Display the basic contents of the cooperative mode like the player2 ship, fire, the titles, etc
coRender :: AsteroidsGame -> Picture
coRender game = pictures
   ([
      mkShip (isThrusting player) (plColor player) (plLocation player) $ (degree player) | player <- (players game) -- Belal Check This  <-- :)
   ]
   ++
   [
      mkFire (projectiles player) | player <- (players game)
   ]
   ++
   [
      mkTitles (highScore player) (score player) (lives player) (plColor player) (pID player)| player <- (players game)
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
    mkTitles :: Float -> Float -> Float -> Color -> Int -> Picture
    mkTitles hs s l col 1 = pictures
     [
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (gHeight game*2.2) $ color white (text ("High Score: " ++ show (hs)))),
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (gHeight game*2) $ color white (text ("Score: " ++ show (s)))),
      scale (0.2) (0.2) (translate (-(gWidth game*2.3)) (-(gHeight game*2.2)) $ color white (text "Lives: " )),
      showLives l col 1
     ]
    mkTitles hs s l col 2 = pictures
     [
      scale (0.2) (0.2) (translate ((gWidth game*1.2)) (gHeight game*2.2) $ color white (text ("High Score: " ++ show (hs)))),
      scale (0.2) (0.2) (translate ((gWidth game*1.2)) (gHeight game*2) $ color white (text ("Score: " ++ show (s)))),
      scale (0.2) (0.2) (translate ((gWidth game*1.2)) (-(gHeight game*2.2)) $ color white (text "Lives: " )),
      showLives l col 2
     ]

    -- | Display the number of player's lives
    showLives :: Float -> Color -> Int -> Picture
    showLives n col 1 = pictures [scale (0.8) (0.8) (mkShip False col ((-gWidth game)*(0.57) + 70 + x*40,(-gHeight game)*(0.55) + 32) 270) | x <- [1..n] ]
    showLives n col 2 = pictures [scale (0.8) (0.8) (mkShip False col ((gWidth game)*(0.30) + 70 + x*40,(-gHeight game)*(0.55) + 32) 270) | x <- [1..n] ]