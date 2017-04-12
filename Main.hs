module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 1000
height = 700
offset = 100

window :: Display
window = InWindow "Asteroids" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-------------------- the custom datatypes --  TODO :: decide if we should use deriving Show 
data Player = Player 
    { degree :: Float            --  the degree will be W.R.T X-axis like this   (>)  <- space ship at degree 0
    , plSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
    , plLocation :: (Float, Float) -- location W.R.T (x and y axes)
    , rotating :: Int
    , thrust :: Bool
    , firing :: Int
    }


data Projectile = Projectile
     { prSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
     , prLocation :: (Float, Float) -- location W.R.T (x and y axes)
     }


data Asteroid = Asteroid
     { size :: Int
     , aLocation :: (Float, Float) -- location W.R.T (x and y axes)
     , aSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
     , radius :: Float        -- decide on whatever the team see right (circle or quadrilateral)
     }

data AsteroidsGame = Game  
     { player :: Player
     , projectiles :: [Projectile]
     , asteroids   :: [Asteroid]
     }


------------------- Basic Functions --

--initialize the sta
initialState :: AsteroidsGame
initialState = Game
   { player = initializePlayer
     , projectiles = []
     , asteroids   = []
   }

initializePlayer :: Player
initializePlayer = Player
    { degree = 250
    , plSpeed = (0,0)
    , plLocation = (0,0)
    }

-- the game foreach loop
update :: Float -> AsteroidsGame -> AsteroidsGame 
update seconds as = as

-- handle game events like thrust button etc
handleKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleKeys _ game = game

render :: AsteroidsGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
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

