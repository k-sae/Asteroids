module DataTypes where
import Graphics.Gloss

-------------------- the custom datatypes --  TODO :: decide if we should use deriving Show 
-- | Game constants int type
width, height, offset, fireDelay, projectileLife :: Int
width = 1000
height = 700
offset = 100
fireDelay = 15
projectileLife = 400
awidth ,aheight :: Float 
awidth =  fromIntegral ((width `div` 2)-10)
aheight = fromIntegral (height `div` 2)
-- | Game constants float type
rotationSpeed, accelerateSpeed, thrustMaxSpeed, asteroidMaxSpeed, asteroidNo :: Float
rotationSpeed = 3.0
accelerateSpeed = 0.2
thrustMaxSpeed = 6 
asteroidMaxSpeed = 4
asteroidNo = 3

-- | player data type
data Player = Player 
    { pID :: Int                 -- ^ player id
    , degree :: Float            -- ^ the degree will be W.R.T X-axis like this   (>)  <- space ship at degree 0
    , plSpeed  :: (Float, Float)  -- ^ speed W.R.T (x and y axes)
    , plLocation :: (Float, Float) -- ^ location W.R.T (x and y axes)
    , rotatingBy :: Float          -- ^ rotation speed 
    , firingSpeed :: Int               -- ^ fire speed for more fun :D
    , isrotating :: Bool               -- ^ rotation state
    , isFiring :: Bool                 -- ^ fire state
    , projectiles :: [Projectile]      -- ^ player projectiles
    , firemode :: Int                  -- ^ fire mode
    , plColor :: Color                 -- ^ color
    , isThrusting :: Bool              -- ^ thrust state
    , score :: Float                    -- ^ player score
    , highScore :: Float                -- ^ player highest score
    , lives :: Float                    -- ^ remaining lives
    , isDead :: Bool
    } deriving (Eq)

-- | projectile data type
data Projectile = Projectile
     { prSpeed  :: (Float, Float)  -- ^ speed W.R.T (x and y axes)
     , prLocation :: (Float, Float) -- ^ location W.R.T (x and y axes)
     , prLifeTime :: Int            -- ^ projectile life time
     } deriving (Eq)


data Asteroid = Asteroid
     { size :: Int                 -- ^ asteroid size
     , aLocation :: (Float, Float) -- ^ location W.R.T (x and y axes)
     , aSpeed  :: (Float, Float)  -- ^ speed W.R.T (x and y axes)
     , radius :: Float        -- ^ decide on whatever the team see right (circle or quadrilateral)
     } deriving (Eq)

data AsteroidsGame = Game 
     { players :: [Player]       -- ^ players in game
     , gameMode :: GameMode        -- ^ game mode 
     , gWidth :: Float             -- ^ game width
     , gHeight :: Float           -- ^ game height
     , asteroids   :: [Asteroid]  -- ^ asteroids in game
     }
-- | all possible game modes 
data GameMode = Menu | Pause | GameOver | Single | Cooperative | Versus deriving Eq  -- deriving Eq so the gamemodes will be comparable

-- | holder that holds game collsion updates 
data Holder = Holder 
     { hProjectiles :: [Projectile] ,
       hAsteroids :: [Asteroid],
       noOfCollision :: Float,
       hPlayer :: Player
     }

