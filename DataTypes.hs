module DataTypes where
import Graphics.Gloss

-------------------- the custom datatypes --  TODO :: decide if we should use deriving Show 
data Player = Player 
    { degree :: Float            --  the degree will be W.R.T X-axis like this   (>)  <- space ship at degree 0
    , plSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
    , plLocation :: (Float, Float) -- location W.R.T (x and y axes)
    , rotatingBy :: Float
    , firingSpeed :: Int               -- fire speed for more fun :D
    , isrotating :: Bool
    , isFiring :: Bool
    , projectiles :: [Projectile]
    , firemode :: Int                  -- fire mode may be removed later 
    , plColor :: Color
    }


data Projectile = Projectile
     { prSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
     , prLocation :: (Float, Float) -- location W.R.T (x and y axes)
     , prLifeTime :: Int
     }


data Asteroid = Asteroid
     { size :: Int
     , aLocation :: (Float, Float) -- location W.R.T (x and y axes)
     , aSpeed  :: (Float, Float)  -- speed W.R.T (x and y axes)
     , radius :: Float        -- decide on whatever the team see right (circle or quadrilateral)
     }

data AsteroidsGame = Game 
     { players :: [Player]
     , gameMode :: GameMode
     , asteroids   :: [Asteroid]
     }

data GameMode = Menu | Single | Cooperative | Versus deriving Eq  -- deriving Eq so the gamemodes will be comparable
