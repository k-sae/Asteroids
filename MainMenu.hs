module MainMenu where
import DataTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
updateMenue :: Float -> AsteroidsGame -> AsteroidsGame 
updateMenue seconds game = game


handleMenuKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleMenuKeys (EventKey (Char '1') _ _ _) game                            -- Enter singleplayer mode when press '1'
 | (gameMode game) == Menu = game {gameMode = Single}
                   -- Only if he is in Menu mode
handleMenuKeys (EventKey (Char '2') _ _ _) game                            -- Enter cooperative mode when press '2'
 | (gameMode game) == Menu = game {gameMode = Cooperative}
              -- Only if he is in Menu mode
handleMenuKeys (EventKey (Char '3') _ _ _) game                            -- Enter versus mode when press '3'
 | (gameMode game) == Menu = game {gameMode = Versus}                  -- Only if he is in Menu mode
handleMenuKeys (EventKey (Char 'q') _ _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'
handleMenuKeys _ game = game