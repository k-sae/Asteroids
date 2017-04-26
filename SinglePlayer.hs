module SinglePlayer where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Shapes
import Asteroids
import Player
import System.Random
----------Game Updates
updateSinglePlayerGame :: Float -> AsteroidsGame -> AsteroidsGame 
updateSinglePlayerGame seconds = updateGamePlayersStates 

-- 'Function Composition'
updateGamePlayersStates :: AsteroidsGame -> AsteroidsGame 
updateGamePlayersStates game  = game {players = updatePlayers game
                                     ,asteroids = [updateAsteroid asteroid | asteroid <- (asteroids game)] } 

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
      --polygon [(0,0),(0,40),(20,80),(80,80),(100,0),(0,0)],
      --color white (polygon [(2,2),(2,38),(22,78),(78,78),(98,2),(2,2)])
      --circleSolid 80,
      --translate 20 20 (circle 10),
      --translate (-20) (-20) (circle 10),
      --translate (-20) (20) (circle 10),
      --translate (20) (-20) (circle 10)
      mkAst (radius asteroid) (aLocation asteroid) | asteroid <- (asteroids game)
   ]
   ++
   [
      mkShip (isThrusting player) (plColor player) (plLocation player) $ (degree player) | player <- (players game) -- Belal Check This  <-- :)
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

    mkAst :: Float -> (Float, Float) -> Picture
    mkAst r (x,y) = pictures
     [
       scale 1 (0.8) (translate x y $ color (greyN 0.2) (circleSolid r))
     ]

    mkStars :: Int -> Picture
    mkStars n = pictures
     [
       translate (fst l) (snd l) $ color blue (circleSolid 2) | l <- getVal (randX n) (randY n)
     ]

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