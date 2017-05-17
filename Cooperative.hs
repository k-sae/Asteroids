module Cooperative where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Shapes
import Asteroids
import Player
import System.Random
----------Game Updates
updateCooperativeGame :: AsteroidsGame -> AsteroidsGame 
updateCooperativeGame  = updateGamePlayersStates . initializeTwoPlayer

initializeTwoPlayer :: AsteroidsGame -> AsteroidsGame
initializeTwoPlayer game | length(players game) <= 1 = game{players = initializePlayers 2}
                         | otherwise = game

-- 'Function Composition'
updateGamePlayersStates :: AsteroidsGame -> AsteroidsGame 
updateGamePlayersStates game  = game {players = updatePlayers game
                                     ,asteroids = [updateAsteroid asteroid game| asteroid <- updateAsteroidList(asteroids game)] } 

--------Events Hndling
handleCooperativeKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleCooperativeKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { players = updateRotationStates (-rotationSpeed) True (players game) 1}    -- Rotate the ship Clock-Wise when press 'd'
handleCooperativeKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { players = updateRotationStates (-rotationSpeed) False (players game) 1}
handleCooperativeKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { players = updateRotationStates (rotationSpeed) True (players game) 1}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleCooperativeKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { players = updateRotationStates (rotationSpeed) False (players game) 1}
handleCooperativeKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {players = updateThrustStatus (players game) True 1 0}
handleCooperativeKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {players = updateThrustStatus (players game) False 1 0}
handleCooperativeKeys (EventKey (SpecialKey KeyCtrlR) Down _ _) game = game {players = updateFireStatus (players game) True 1 0}
handleCooperativeKeys (EventKey (SpecialKey KeyCtrlR) Up _ _) game = game {players = updateFireStatus (players game) False 1 0}
handleCooperativeKeys _ game = game

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

    showLives :: Float -> Color -> Int -> Picture
    showLives n col 1 = pictures [scale (0.8) (0.8) (mkShip False col ((-gWidth game)*(0.57) + 70 + x*40,(-gHeight game)*(0.55) + 32) 270) | x <- [1..n] ]
    showLives n col 2 = pictures [scale (0.8) (0.8) (mkShip False col ((gWidth game)*(0.30) + 70 + x*40,(-gHeight game)*(0.55) + 32) 270) | x <- [1..n] ]