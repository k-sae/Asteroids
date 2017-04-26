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
initialState = Game
   { players = initializePlayers
    , gameMode = Menu
    , gWidth = (fromIntegral width)
    , gHeight = (fromIntegral height)
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
    , plColor = white
    , isThrusting = False
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

 | otherwise = pictures
   ([
      --assume this value is the number of stars
      mkStars 111
   ]
   ++
   [
    mkShip (isThrusting player) (plColor player) (plLocation player) $ (degree player) | player <- (players game) -- Belal Check This  <-- :)
   ])
   where
    mkShip :: Bool -> Color -> (Float, Float) -> Float -> Picture
    mkShip False col (x,y) degree = pictures
     [
       translate x y $ color col $ sectorWire (degree-20) (degree+20) 40
       --translate x y $ color col $ solidArc (degree-15) (degree+15) 38
     ]
    mkShip True col (x,y) degree = pictures
     [
       translate x y $ color red $ solidArc (degree-5) (degree+5) 45,
       translate x y $ color col $ solidArc (degree-20) (degree+20) 40,
       translate x y $ color black $ solidArc (degree-18) (degree+18) 39
     ]

    mkStars :: Int -> Picture
    mkStars n = pictures
     [
       translate (fst l) (snd l) $ color white (circleSolid 2) | l <- getVal (randX n) (randY n)
     ]

    randX :: Int -> [Float]
    randX n = take n (randomRs ((-(gWidth game)), (gWidth game) :: Float) (mkStdGen n))
    randY :: Int -> [Float]
    randY n = take n (randomRs ((-(gHeight game)), (gHeight game) :: Float) (mkStdGen (n*2)))

    getVal :: [Float] -> [Float] -> [(Float,Float)] 
    getVal [] [] = []
    getVal [] _ = []
    getVal _ [] = []
    getVal (x:xs) (y:ys) = (x,y) : (getVal xs ys)