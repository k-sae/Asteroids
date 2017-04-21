module MainMenu where
import DataTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
updateMenu :: Float -> AsteroidsGame -> AsteroidsGame 
updateMenu seconds game = game


handleMenuKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleMenuKeys (EventKey (Char '1') _ _ _) game =  game {gameMode = Single}  -- Enter singleplayer mode when press '1'
                                               
                   -- Only if he is in Menu mode
handleMenuKeys (EventKey (Char '2') _ _ _) game  = game {gameMode = Cooperative}  -- Enter cooperative mode when press '2'

handleMenuKeys (EventKey (Char '3') _ _ _) game  = game {gameMode = Versus}   -- Enter versus mode when press '3'
                                             
handleMenuKeys _ game = game