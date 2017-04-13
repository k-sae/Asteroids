module MainMenu where
import DataTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
updateMenue :: Float -> AsteroidsGame -> AsteroidsGame 
updateMenue seconds game = game


handleMenuKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleMenuKeys (EventKey (Char '1') _ _ _) game =  game {gameMode = Single}  -- Enter singleplayer mode when press '1'
                                               
                   -- Only if he is in Menu mode
handleMenuKeys (EventKey (Char '2') _ _ _) game  = game {gameMode = Cooperative}  -- Enter cooperative mode when press '2'

handleMenuKeys (EventKey (Char '3') _ _ _) game  = game {gameMode = Versus}   -- Enter versus mode when press '3'
                                              
handleMenuKeys (EventKey (Char 'q') _ _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'
handleMenuKeys _ game = game