module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Shapes
import DataTypes
import MainMenu
import Pause
import SinglePlayer
import Asteroids
import System.Random
--functions in this file is responsible for finding the appropriate function according to mode
-- see 'update' function as example 
window :: Display
window = InWindow "Asteroids" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

------------------- Basic Functions --

--initialize the states of the game
initialState :: AsteroidsGame
initialState    = Game
   { players    = initializePlayers
    , gameMode  = Menu
    , gWidth    = (fromIntegral width)
    , gHeight   = (fromIntegral height)
    , asteroids = initializeAsteroids
   }

initializePlayers :: [Player]
initializePlayers = [Player                  -- idk how this worked but it did :D 
    { projectiles = []
    , degree      = 0
    , plSpeed     = (0,0)
    , plLocation  = (0,0)
    , rotatingBy  = rotationSpeed
    , firingSpeed = 10
    , isrotating  = False
    , isFiring    = False
    , firemode    = 1
    , plColor     = (makeColorI 51 122 183 255)
    , isThrusting = False
    , score       = 0
    , highScore   = 0
    , lives       = 3
    }]

initializeAsteroids :: [Asteroid]
initializeAsteroids = [Asteroid                  -- idk how this worked but it did :D 
    { size = 10
    , aLocation = (0,0)
    , aSpeed = (0,0)
    , radius = 100
    }]

-- the game foreach loop
update :: Float -> AsteroidsGame -> AsteroidsGame                        -- update the game according to the Game Mode
update seconds game | (gameMode game) == Menu = updateMenu seconds initialState  -- call the update menue from MainMenu.hs file
                    | (gameMode game) == Pause = updatePause seconds game
                    | otherwise = SinglePlayer.updateSinglePlayerGame seconds game

-- handle game events like thrust button etc


--EventKey Key KeyState Modifiers (Float, Float) 
--ref:: https://hackage.haskell.org/package/gloss-1.11.1.1/docs/Graphics-Gloss-Interface-IO-Game.html
handleKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleKeys event game
                       | mode == Menu = handleMenuKeys event game          --same as the update function here u call the appropriate key events 
                       | mode == Pause = handlePauseKeys event game
                       | mode == Single = handleSingleplayerKeys event game
                       | otherwise = game
                  where mode = gameMode game


render :: AsteroidsGame  --- update the render like the update function in order to behave like the update function
       -> Picture   --TODO BELBEL splite render function to different files for better organization
render game 
 | (gameMode game) == Menu = menuRender game

 | (gameMode game) == Pause = pauseRender game

 | otherwise = spRender game
