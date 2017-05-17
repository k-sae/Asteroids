module GameOver where
import DataTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- | Update the gameover screen.
updateGameOver :: Float -> AsteroidsGame -> AsteroidsGame 
updateGameOver seconds game = game

-- | Handle the key events on the gameover screen.
handleGameOverKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleGameOverKeys (EventKey (Char 'q') _ _ _) game  = game {gameMode = Menu}
handleGameOverKeys _ game = game

-- | Display the gameover screen.
gameOverRender :: AsteroidsGame -> Picture
gameOverRender game = color white (pictures
 [
   translate (-350) 280 (text "-------"),
   translate (-350) 200 (text "|.GameOver.|"),
   translate (-350) 120 (text "-------"),
   scale (0.3) (0.3) (translate (-1000) (60) (text "Press 'q' to return to MainMenu"))
 ])