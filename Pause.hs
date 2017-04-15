module Pause where
import DataTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
updatePause :: Float -> AsteroidsGame -> AsteroidsGame 
updatePause seconds game = game


handlePauseKeys :: Event -> AsteroidsGame -> AsteroidsGame
handlePauseKeys (EventKey (Char '1') _ _ _) game =  game {gameMode = Single}       -- Continue the game as singleplayer mode when press '1'

handlePauseKeys (EventKey (Char '2') _ _ _) game  = game {gameMode = Cooperative}  -- Continue the game as cooperative mode when press '2'

handlePauseKeys (EventKey (Char '3') _ _ _) game  = game {gameMode = Versus}       -- Continue the game as versus mode when press '3'

handlePauseKeys (EventKey (Char 'q') _ _ _) game  = game {gameMode = Menu}         -- Return to the menu and quit the game when press 'q'                                    

handlePauseKeys _ game = game