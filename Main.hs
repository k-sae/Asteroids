module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Shapes
import DataTypes
import MainMenu
import Pause
import General
import SinglePlayer
import Player
import Cooperative
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
   { players    = []
    , gameMode  = Menu
    , gWidth    = (fromIntegral width)
    , gHeight   = (fromIntegral height)
    , asteroids = initializeAsteroids 3
   }



-- the game foreach loop
update :: Float -> AsteroidsGame -> AsteroidsGame                        -- update the game according to the Game Mode
update seconds game | (gameMode game) == Menu = updateMenu seconds initialState  -- call the update menue from MainMenu.hs file
                    | (gameMode game) == Pause = updatePause seconds game
                    | otherwise = General.updateGeneralGame seconds game

-- handle game events like thrust button etc


--EventKey Key KeyState Modifiers (Float, Float) 
--ref:: https://hackage.haskell.org/package/gloss-1.11.1.1/docs/Graphics-Gloss-Interface-IO-Game.html
handleKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleKeys event game
                       | mode == Menu = handleMenuKeys event game          --same as the update function here u call the appropriate key events 
                       | mode == Pause = handlePauseKeys event game
                       | otherwise = handleGeneralKeys event game
                  where mode = gameMode game


render :: AsteroidsGame  --- update the render like the update function in order to behave like the update function
       -> Picture   --TODO BELBEL splite render function to different files for better organization
render game 
 | (gameMode game) == Menu = menuRender game

 | (gameMode game) == Pause = pauseRender game

-- | (gameMode game) == Single = spRender game

-- | (gameMode game) == Cooperative = coRender game

 | otherwise = generalRender game
