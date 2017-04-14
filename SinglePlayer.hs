module SinglePlayer where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Graphics.Gloss.Geometry.Angle
----------Game Updates
updateSinglePlayerGame :: Float -> AsteroidsGame -> AsteroidsGame 
updateSinglePlayerGame seconds = updateLocation . updateRotationSpeed 

updateRotationSpeed :: AsteroidsGame -> AsteroidsGame 
updateRotationSpeed game  = game {players = [rotateBy x |x <-(players game)] }

rotateBy :: Player -> Player 
rotateBy player |(isrotating player) == True =  player {degree = newdegree}
                | otherwise = player
 where newdegree = (rotatingBy player) + (degree player)

updateLocation :: AsteroidsGame -> AsteroidsGame
updateLocation game = game {players = [updateLocationBy player |player <-(players game)] }

updateLocationBy player = player {plLocation = newLocation (plLocation player)}
                       where newLocation (x,y) = (x + xvelocity (plSpeed player), y + yvelocity (plSpeed player))
                             xvelocity (x,_) = x
                             yvelocity (_,y) = y

--------Events Hndling
handleSingleplayerKeys (EventKey (Char 'd') Down _ _) game = game { players = updateRotationStates (-10) True (players game) 0}    -- Rotate the ship Clock-Wise when press 'd'
handleSingleplayerKeys (EventKey (Char 'd') Up _ _) game = game { players = updateRotationStates (-10) False (players game) 0}

handleSingleplayerKeys (EventKey (Char 'a') Down _ _) game = game { players = updateRotationStates (10) True (players game) 0}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleSingleplayerKeys (EventKey (Char 'a') Up _ _) game = game { players = updateRotationStates (10) False (players game) 0}
handleSingleplayerKeys (EventKey (Char 'q') _ _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'
handleSingleplayerKeys (EventKey (Char 'w') _ _ _) game = game {players = [ updateSpeed player | player <- (players game)] }
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
updateSpeed :: Player -> Player
updateSpeed player = player{plSpeed = newSpeed (plSpeed player)}
            where newSpeed (x,y) = (x + (cos (degToRad (degree player))), degToRad (y + sin (degree player)))

