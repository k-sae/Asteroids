module MainMenu where
import DataTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- | Update the mainmenu screen.
updateMenu :: Float -> AsteroidsGame -> AsteroidsGame -> AsteroidsGame 
updateMenu seconds game initial = initial{gWidth = (gWidth game), gHeight = (gHeight game)}

-- | Handle the key events on the mainmenu like enter the single player mode, etc.
handleMenuKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleMenuKeys (EventKey (Char '1') _ _ _) game =  game {gameMode = Single}  -- Enter singleplayer mode when press '1'
handleMenuKeys (EventKey (Char '2') _ _ _) game  = game {gameMode = Cooperative}  -- Enter cooperative mode when press '2'
--handleMenuKeys (EventKey (Char '3') _ _ _) game  = game {gameMode = Versus}   -- Enter versus mode when press '3'
handleMenuKeys (EventResize (w,h)) game = game {gWidth = (fromIntegral w) , gHeight = (fromIntegral h)}                           
handleMenuKeys _ game = game

-- | Display the mainmenu with some options.
menuRender :: AsteroidsGame -> Picture
menuRender game = color white (pictures
 [
   translate (-400) 280 (text "--------"),
   translate (-400) 200 (text "| Asteroids. |"),
   translate (-400) 120 (text "--------"),
   scale (0.5) (0.5) (translate (-450) 100 (text "(1)SinglePlayer")),
   scale (0.5) (0.5) (translate (-450) (-100) (text "(2)Cooperative"))
   --,scale (0.5) (0.5) (translate (-450) (-300) (text "(3)Versus"))
 ])