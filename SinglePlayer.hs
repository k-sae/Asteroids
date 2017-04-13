module SinglePlayer where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes


updateSinglePlayerGame :: Float -> AsteroidsGame -> AsteroidsGame 
updateSinglePlayerGame seconds game = updateRotationSpeed game

updateRotationSpeed :: AsteroidsGame -> AsteroidsGame 
updateRotationSpeed game 
                        | isrotating (player game) == True = game {player =rotateBy (player game) } 
                        | otherwise = game
rotateBy :: Player -> Player 
rotateBy player = player {degree = newdegree}
 where newdegree = (rotatingBy player) + (degree player)

handleSingleplayerKeys (EventKey (Char 'd') Down _ _) game = game { player = updateRotationStates (-10) True (player game)}    -- Rotate the ship Clock-Wise when press 'd'
handleSingleplayerKeys (EventKey (Char 'd') Up _ _) game = game { player = updateRotationStates (-10) False (player game)}

handleSingleplayerKeys (EventKey (Char 'a') Down _ _) game = game { player = updateRotationStates (10) True (player game)}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleSingleplayerKeys (EventKey (Char 'a') Up _ _) game = game { player = updateRotationStates (10) False (player game)}
handleSingleplayerKeys _ game = game

--The x value will be the rotatingBy value!
updateRotationStates :: Float -> Bool -> Player -> Player
updateRotationStates x rotationState player = player {isrotating = rotationState , rotatingBy = x}

