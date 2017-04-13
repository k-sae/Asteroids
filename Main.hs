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
handleKeys _ game = game

render :: AsteroidsGame  --- update the render like the update function in order to behave like the the update function
       -> Picture   
render game = pictures
 [
   mkShip black white (plLocation (player game)) $ (degree (player game))
 ]
 where
  mkShip :: Color -> Color -> (Float, Float) -> Float -> Picture
  mkShip col col2 (x,y) degree = pictures
   [
     translate x y $ color red $ arcSolid (degree+15) (degree+25) 45,
     translate x y $ color col $ arcSolid degree (degree+40) 40,
     translate x y $ color col2 $ arcSolid (degree+10) (degree+30) 35

   ]

