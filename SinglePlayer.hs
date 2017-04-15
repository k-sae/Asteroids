module SinglePlayer where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Graphics.Gloss.Geometry.Angle


----------Game Updates
updateSinglePlayerGame :: Float -> AsteroidsGame -> AsteroidsGame 
updateSinglePlayerGame seconds = updateLocation . updateGamePlayersStates 

updateGamePlayersStates :: AsteroidsGame -> AsteroidsGame 
updateGamePlayersStates game  = game {players = [(updateSpeed.rotateBy) x |x <-(players game)] }

rotateBy :: Player -> Player 
rotateBy player |(isrotating player) == True =  player {degree = newdegree}
                | otherwise = player
 where newdegree = (rotatingBy player) + (degree player)

updateLocation :: AsteroidsGame -> AsteroidsGame
updateLocation game = game {players = [(updateLocationBy game player )|player <-(players game)] }
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
--------Events Hndling
handleSingleplayerKeys (EventKey (Char 'd') Down _ _) game = game { players = updateRotationStates (-rotationSpeed) True (players game) 0}    -- Rotate the ship Clock-Wise when press 'd'
handleSingleplayerKeys (EventKey (Char 'd') Up _ _) game = game { players = updateRotationStates (-rotationSpeed) False (players game) 0}

handleSingleplayerKeys (EventKey (Char 'a') Down _ _) game = game { players = updateRotationStates (rotationSpeed) True (players game) 0}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleSingleplayerKeys (EventKey (Char 'a') Up _ _) game = game { players = updateRotationStates (rotationSpeed) False (players game) 0}
handleSingleplayerKeys (EventKey (Char 'p') Down _ _) game = game {gameMode = Pause}   -- Pause the game when press 'p'
handleSingleplayerKeys (EventKey (Char 'q') Down _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'
handleSingleplayerKeys (EventKey (Char 'w') Down _ _) game = game {players = updateThrustStatus (players game) True 0 0}
handleSingleplayerKeys (EventKey (Char 'w') Up _ _) game = game {players = updateThrustStatus (players game) False 0 0}
handleSingleplayerKeys (EventResize (w,h)) game = game {gWidth = w , gHeight = h}
handleSingleplayerKeys _ game = game

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

updateSpeed :: Player -> Player
updateSpeed player | isThrusting player == True = player{plSpeed = newSpeed (plSpeed player)}
                   | otherwise = player
            where newSpeed (x,y) = (check (x + (cos (degToRad ((degree player) - 180))) * accelerateSpeed),check (y + (sin (degToRad ((degree player)-180))) * accelerateSpeed))
                  check x  | x > thrustMaxSpeed = thrustMaxSpeed
                           | x < -thrustMaxSpeed = -thrustMaxSpeed
                           |otherwise  = x

