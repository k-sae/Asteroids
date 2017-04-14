module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Shapes
import DataTypes
import MainMenu
import SinglePlayer
width, height, offset, thrustMaxSpeed :: Int
width = 1000
height = 700
offset = 100
thrustMaxSpeed = 400       -- the thrust will speed up till reach max value
rotationSpeed :: Float
rotationSpeed = 10
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
   { players = initializePlayers
    , gameMode = Menu
    , asteroids   = []
   }

initializePlayers :: [Player]
initializePlayers = [Player                  -- idk how this worked but it did :D 
    { projectiles = []
    , degree = 0
    , plSpeed = (0,0)
    , plLocation = (0,0)
    , rotatingBy = rotationSpeed
    , firingSpeed  = 10
    , isrotating = False
    , isFiring    = False
    , firemode = 1
    , plColor = black
    }]

-- the game foreach loop
update :: Float -> AsteroidsGame -> AsteroidsGame                        -- update the game according to the Game Mode
update seconds game | (gameMode game) == Menu = updateMenue seconds game
                    | otherwise = updateSinglePlayerGame seconds game

-- handle game events like thrust button etc


--EventKey Key KeyState Modifiers (Float, Float) 
--ref:: https://hackage.haskell.org/package/gloss-1.11.1.1/docs/Graphics-Gloss-Interface-IO-Game.html
handleKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleKeys event game
                       | mode == Menu = handleMenuKeys event game
                       | mode == Single = handleSingleplayerKeys event game
                       | otherwise = game
                  where mode = gameMode game


render :: AsteroidsGame  --- update the render like the update function in order to behave like the update function
       -> Picture   
render game 
 | (gameMode game) == Menu = pictures
   [
     translate (-450) 200 (text "(1)SinglePlayer"),
     translate (-450) 0 (text "(2)Cooperative"),
     translate (-450) (-200) (text "(3)Versus")   -- fixed this
   ]

 | otherwise = pictures
   [
    mkShip (plColor player) (plLocation player) $ (degree player) | player <- (players game) -- Belal Check This  <-- :)
   ]
   where
    mkShip :: Color ->(Float, Float) -> Float -> Picture
    mkShip col (x,y) degree = pictures
     [
        translate x y $ color col $ solidArc (degree+250) (degree+290) 40
      , translate x y $ color white $ solidArc (degree+260) (degree+280) 35

     ]


