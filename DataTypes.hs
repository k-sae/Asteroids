module DataTypes where
import Graphics.Gloss

-------------------- the custom datatypes --  TODO :: decide if we should use deriving Show 
width, height, offset, fireDelay, projectileLife :: Int
width = 1000
height = 700
offset = 100
fireDelay = 15
projectileLife = 400

      -- the thrust will speed up till reach max value
awidth ,aheight :: Float 
awidth =  fromIntegral ((width `div` 2)-10)
aheight = fromIntegral (height `div` 2)
rotationSpeed, accelerateSpeed, thrustMaxSpeed, asteroidMaxSpeed, asteroidNo :: Float
rotationSpeed = 3.0
accelerateSpeed = 0.2
thrustMaxSpeed = 6 
asteroidMaxSpeed = 4
asteroidNo = 3


data Player = Player 
    { pID :: Int
    , degree :: Float            --  the degree will be W.R.T X-axis like this   (>)  <- space ship at degree 0
    , plSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
    , plLocation :: (Float, Float) -- location W.R.T (x and y axes)
    , rotatingBy :: Float
    , firingSpeed :: Int               -- fire speed for more fun :D
    , isrotating :: Bool
    , isFiring :: Bool
    , projectiles :: [Projectile]
    , firemode :: Int                  -- fire mode may be removed later 
    , plColor :: Color
    , isThrusting :: Bool
    , score :: Float
    , highScore :: Float
    , lives :: Float
    } deriving (Eq)

-- idk
data Projectile = Projectile
     { prSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
     , prLocation :: (Float, Float) -- location W.R.T (x and y axes)
     , prLifeTime :: Int
     } deriving (Eq)


data Asteroid = Asteroid
     { size :: Int
     , aLocation :: (Float, Float) -- location W.R.T (x and y axes)
     , aSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
     , radius :: Float        -- decide on whatever the team see right (circle or quadrilateral)
     } deriving (Eq)

data AsteroidsGame = Game 
     { players :: [Player]
     , gameMode :: GameMode
     , gWidth :: Float
     , gHeight :: Float
     , asteroids   :: [Asteroid]
     }
data CollisionItem = CollisionItem 
     { cPlayers :: Player,
       cAsteroid :: Asteroid
     }
data GameMode = Menu | Pause | GameOver | Single | Cooperative | Versus deriving Eq  -- deriving Eq so the gamemodes will be comparable


data Holder = Holder 
     { hProjectiles :: [Projectile] ,
       hAsteroids :: [Asteroid],
       noOfCollision :: Float,
       hPlayer :: Player
     }

