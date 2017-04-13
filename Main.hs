module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import MainMenu
import SinglePlayer
width, height, offset :: Int
width = 1000
height = 700
offset = 100
thrustMaxSpeed = 400       -- the thrust will speed up till reach max value
window :: Display
window = InWindow "Asteroids" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

------------------- Basic Functions --

--initialize the states of the game
initialState :: AsteroidsGame
initialState = Game
   { player = initializePlayer
    , gameMode = Menu
    , asteroids   = []
   }

initializePlayer :: Player
initializePlayer = Player
    { projectiles = []
    , degree = 250
    , plSpeed = (0,0)
    , plLocation = (0,0)
    , rotatingBy = 0
    , firingSpeed  = 10
    , isrotating = False
    , isFiring    = False
    , firemode = 1
    }

-- the game foreach loop
update :: Float -> AsteroidsGame -> AsteroidsGame                        -- update the game according to the Game Mode
update seconds game | (gameMode game) == Menu = updateMenue seconds game
                    | otherwise = updateSinglePlayerGame seconds game

-- handle game events like thrust button etc
handleKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleKeys (EventKey (Char '1') _ _ _) game                            -- Enter singleplayer mode when press '1'
 | (gameMode game) == Menu = game {gameMode = Single}                  -- Only if he is in Menu mode
handleKeys (EventKey (Char '2') _ _ _) game                            -- Enter cooperative mode when press '2'
 | (gameMode game) == Menu = game {gameMode = Cooperative}             -- Only if he is in Menu mode
handleKeys (EventKey (Char '3') _ _ _) game                            -- Enter versus mode when press '3'
 | (gameMode game) == Menu = game {gameMode = Versus}                  -- Only if he is in Menu mode
handleKeys (EventKey (Char 'q') _ _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'

handleKeys (EventKey (Char 'd') _ _ _) game                                -- Rotate the ship Clock-Wise when press 'd'
 | (gameMode game) /= Menu = game { player = rotatePl (-10) (player game)} -- Only if he is not in Menu mode
handleKeys (EventKey (Char 'a') _ _ _) game                                -- Rotate the ship Anti_Clock-Wise when press 'a'
 | (gameMode game) /= Menu = game { player = rotatePl (10) (player game)}  -- Only if he is not in Menu mode

handleKeys _ game = game

--The x value will be the rotatingBy value!
rotatePl :: Float -> Player -> Player
rotatePl x player = player {degree = newdegree}
 where 
  newdegree = ((degree player) + x )

render :: AsteroidsGame  --- update the render like the update function in order to behave like the update function
       -> Picture   
render game 
 | (gameMode game) == Menu = pictures
   [
     translate (-450) 200 (text "(1)SinglePlayer"),
     translate (-450) 0 (text "(2)Cooperative"),
     translate (-450) (-200) (text "(2)Versus")
   ]

 | otherwise = pictures
   [
     mkShip black black (plLocation (player game)) $ (degree (player game))
   ]
   where
    mkShip :: Color -> Color -> (Float, Float) -> Float -> Picture
    mkShip col col2 (x,y) degree = pictures
     [
       translate x y $ color col $ sectorWire degree (degree+40) 40,
       translate x y $ color col2 $ sectorWire (degree+10) (degree+30) 35

     ]

