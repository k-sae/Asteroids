module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Shapes
import DataTypes
import MainMenu
import Pause
import SinglePlayer

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
    , gWidth = width
    , gHeight = height
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
    , isThrusting = False
    }]

-- the game foreach loop
update :: Float -> AsteroidsGame -> AsteroidsGame                        -- update the game according to the Game Mode
update seconds game | (gameMode game) == Menu = updateMenue seconds initialState
                    | (gameMode game) == Pause = updatePause seconds game
                    | otherwise = updateSinglePlayerGame seconds game

-- handle game events like thrust button etc


--EventKey Key KeyState Modifiers (Float, Float) 
--ref:: https://hackage.haskell.org/package/gloss-1.11.1.1/docs/Graphics-Gloss-Interface-IO-Game.html
handleKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleKeys event game
                       | mode == Menu = handleMenuKeys event game
                       | mode == Pause = handlePauseKeys event game
                       | mode == Single = handleSingleplayerKeys event game
                       | otherwise = game
                  where mode = gameMode game


render :: AsteroidsGame  --- update the render like the update function in order to behave like the update function
       -> Picture   
render game 
 | (gameMode game) == Menu = pictures
   [
     translate (-400) 280 (text "--------"),
     translate (-400) 200 (text "| Asteroids. |"),
     translate (-400) 120 (text "--------"),
     scale (0.5) (0.5) (translate (-450) 100 (text "(1)SinglePlayer")),
     scale (0.5) (0.5) (translate (-450) (-100) (text "(2)Cooperative")),
     scale (0.5) (0.5) (translate (-450) (-300) (text "(3)Versus"))
   ]

 | (gameMode game) == Pause = pictures
   [
     translate (-350) 280 (text "-------"),
     translate (-350) 200 (text "| Paused. |"),
     translate (-350) 120 (text "-------"),
     scale (0.4) (0.4) (translate (-800) (100)  (text "(1)Continue as SinglePlayer")),
     scale (0.4) (0.4) (translate (-800) (-100) (text "(2)Continue as Cooperative")),
     scale (0.4) (0.4) (translate (-800) (-300) (text "(3)Continue as Versus")),
     scale (0.4) (0.4) (translate (-800) (-500) (text "(q)Return to MainMenu"))
   ]

 | otherwise = pictures
   (
    [
      --polygon [(0,0),(0,40),(20,80),(80,80),(100,0),(0,0)],
      --color white (polygon [(2,2),(2,38),(22,78),(78,78),(98,2),(2,2)])
      --circleSolid 80,
      --translate 20 20 (circle 10),
      --translate (-20) (-20) (circle 10),
      --translate (-20) (20) (circle 10),
      --translate (20) (-20) (circle 10)
      mkAst 80 (0,0)
    ]
    ++
    [
      mkShip (isThrusting player) (plColor player) (plLocation player) $ (degree player) | player <- (players game) -- Belal Check This  <-- :)
    ]
   )
   where
    mkShip :: Bool -> Color -> (Float, Float) -> Float -> Picture
    mkShip False col (x,y) degree = pictures
     [
       translate x y $ color col $ solidArc (degree-20) (degree+20) 40,
       translate x y $ color white $ solidArc (degree-10) (degree+10) 35
     ]
    mkShip True col (x,y) degree = pictures
     [
       translate x y $ color red $ solidArc (degree-5) (degree+5) 45,
       translate x y $ color col $ solidArc (degree-20) (degree+20) 40,
       translate x y $ color white $ solidArc (degree-10) (degree+10) 35
     ]

    mkAst :: Float -> (Float, Float) -> Picture
    mkAst r (x,y) = pictures
     [
       scale 1 (0.8) (translate x y $ color (greyN 0.6) (circleSolid r)),
       scale 1 (0.8) (translate (x-(r/3)) (y+(r/3)) $ color (greyN 0.2) (circleSolid (r/5))),
       scale 1 (0.8) (translate (x+(r/3)) (y+(r/3)) $ color (greyN 0.2) (circleSolid (r/5))),
       scale 1 (0.8) (translate (x+(r/3)) (y-(r/3)) $ color (greyN 0.2) (circleSolid (r/5))),
       scale 1 (0.8) (translate (x-(r/3)) (y-(r/3)) $ color (greyN 0.2) (circleSolid (r/5)))
     ]

