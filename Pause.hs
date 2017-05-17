module Pause where
import DataTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- | Update the pause screen.
updatePause :: Float -> AsteroidsGame -> AsteroidsGame 
updatePause seconds game = game

-- | Handle the key events on the pause screen.
handlePauseKeys :: Event -> AsteroidsGame -> AsteroidsGame
--handlePauseKeys (EventKey (Char '1') _ _ _) game =  game {gameMode = Single}       -- Continue the game as singleplayer mode when press '1'
--handlePauseKeys (EventKey (Char '2') _ _ _) game  = game {gameMode = Cooperative}  -- Continue the game as cooperative mode when press '2'
--handlePauseKeys (EventKey (Char '3') _ _ _) game  = game {gameMode = Versus}       -- Continue the game as versus mode when press '3'
handlePauseKeys (EventKey (Char 'q') _ _ _) game  = game {gameMode = Menu}         -- Return to the menu and quit the game when press 'q'                                    
handlePauseKeys (EventKey (Char 'p') Down _ _) game  = game {gameMode = (whereFrom game)}
handlePauseKeys _ game = game

-- | Display the pause screen with some options.
pauseRender :: AsteroidsGame -> Picture
pauseRender game = color white (pictures
 [
   translate (-350) 280 (text "-------"),
   translate (-350) 200 (text "| Paused. |"),
   translate (-350) 120 (text "-------"),
   --scale (0.4) (0.4) (translate (-800) (100)  (text "(1)Continue as SinglePlayer")),
   --scale (0.4) (0.4) (translate (-800) (-100) (text "(2)Continue as Cooperative")),
   --scale (0.4) (0.4) (translate (-800) (-300) (text "(3)Continue as Versus")),
   scale (0.4) (0.4) (translate (-800) (100) (text "(p)Resume")),
   scale (0.4) (0.4) (translate (-800) (-100) (text "(q)Return to MainMenu"))
 ])