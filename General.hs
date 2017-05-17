module General where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import SinglePlayer
import Cooperative
import Shapes
import Asteroids
import Player
import System.Random
import Debug.Trace
----------Game Updates
-- | General Update, like projectile collision update single, cooperative modes
updateGeneralGame :: AsteroidsGame -> AsteroidsGame 
updateGeneralGame  = projectilesCollision.updateGameBasedOnMode

-- | Update the game upon the game mode
updateGameBasedOnMode game  | (gameMode game) == Single = SinglePlayer.updateSinglePlayerGame game
                            | (gameMode game) == Cooperative = Cooperative.updateCooperativeGame game

-- | Update the asteroids - projectile collision
projectilesCollision :: AsteroidsGame -> AsteroidsGame
projectilesCollision game = checkDeadPlayers . updatePlayerCollision (players game) $ game {players = []} 
                         
-- | Update the player - asteroids collision
updatePlayerCollision :: [Player] -> AsteroidsGame -> AsteroidsGame
updatePlayerCollision [] game = game
updatePlayerCollision (p:ps) game = updatePlayerCollision ps $ game { asteroids = (hAsteroids plAstCollision), players = bindPlayers}
                                     where prAstCollision = projectilesCollisionTraverser (projectiles (p)) Holder{hProjectiles = [], hAsteroids = (asteroids game), noOfCollision = 0, hPlayer = p}
                                           bindPlayers = (players game) ++ [(hPlayer plAstCollision)]
                                           plAstCollision = General.updatePlayerAsteroidCollision updateP  (hAsteroids prAstCollision) Holder {hProjectiles = [], hAsteroids = [], noOfCollision = 0, hPlayer = updateP} 
                                           updateP = p {projectiles = (hProjectiles prAstCollision), score = (noOfCollision prAstCollision)*10 + (score p), highScore = newHs, lives = (lives (hPlayer prAstCollision))}
                                           newHs | (score p) > (highScore p) = (score p)
                                                 | otherwise = (highScore p)

-- | Return the dead players
deadPlayers :: AsteroidsGame -> [Player] -> [Player]
deadPlayers game players = [ player | player <- players, (lives player) <=0] 

-- | Check for dead players
checkDeadPlayers :: AsteroidsGame -> AsteroidsGame
checkDeadPlayers game
 | dead == [] = game
 | (gameMode game) == Single =  game{gameMode = GameOver}
 | (gameMode game) == Cooperative = cooperativeDeadPlayers game dead
 | otherwise = game
 where
  dead = (deadPlayers game (players game))

-- Handle the dead players on the cooperative mode
cooperativeDeadPlayers :: AsteroidsGame -> [Player] -> AsteroidsGame
cooperativeDeadPlayers game [x] = game{players = removePlayer x (players game)} -- if one player is dead and the another is not
cooperativeDeadPlayers game dead = game{gameMode = GameOver}

-- | Remove the player when his lives reach to 0
removePlayer :: Player -> [Player] -> [Player]
removePlayer pl (p:ps)
 | pl == p = pl{isDead = True} : ps
 | otherwise = p : removePlayer pl ps 

-- | traverse projectiles to check for collided Objects
projectilesCollisionTraverser ::  [Projectile] -- ^ list of projectiles
 -> Holder -- ^ Empty holder 
 -> Holder -- ^ holds updated game state for analyizing
projectilesCollisionTraverser [] holder = holder
projectilesCollisionTraverser  (pr:prs) holder = projectilesCollisionTraverser prs $ holder {hProjectiles = (hProjectiles holder) ++ (hProjectiles prCollision), hAsteroids = (hAsteroids prCollision), noOfCollision = (noOfCollision holder) + (noOfCollision prCollision)}
                                              where prCollision =  asteroidsCollisionTraverser Holder{hProjectiles = [], hAsteroids = [], noOfCollision = 0, hPlayer = (hPlayer holder)} pr (hAsteroids holder) False

-- | traverse asteroids to check for collided Objects
asteroidsCollisionTraverser :: Holder -- ^ Empty holder 
 -> Projectile -- ^ projectile 
 -> [Asteroid] -- ^ list asteroids that the function will traverse through
 -> Bool -- ^ check if collsion occured through the whole traverse
 -> Holder -- ^ return updated asteroids and projectiles
asteroidsCollisionTraverser holder _ [] True = holder
asteroidsCollisionTraverser holder projectile [] False = holder{ hProjectiles = projectile : (hProjectiles holder)}
-- 2 possible issues up here
--update: i have choosen to ignore them as the fault value will neglictable (nearly 2px)
asteroidsCollisionTraverser holder projectile (a:as) collided
                                                    | distance (aLocation a) (prLocation projectile) <= (radius a) = asteroidsCollisionTraverser holder {hAsteroids =  (hAsteroids holder) ++ breakAsteroid a 2, noOfCollision = (noOfCollision holder) + 1}  projectile as True
                                                    | otherwise = asteroidsCollisionTraverser holder {hAsteroids =  a : (hAsteroids holder)}  projectile as collided
-- | calculate distance between two object using 2d location
--
-- Example: 
-- >>> distance (0,0) (15,30)
-- 33.54102
distance :: (Float, Float) -- ^ location 1
 -> (Float, Float) -- ^ location 2
  -> Float -- ^ distance
distance (x1,y1) (x2,y2) =  sqrt ((x1 - x2)^2 + (y1 - y2)^2)
updatePlayerAsteroidCollision:: Player -> [Asteroid] -> Holder -> Holder
updatePlayerAsteroidCollision _ [] holder = holder
updatePlayerAsteroidCollision player (a:as) holder 
                                                  | distance (aLocation a) playerLocation <= (radius a) = General.updatePlayerAsteroidCollision player as holder { hPlayer = updateCollidedPlayer (hPlayer holder)}  
                                                  | otherwise =   General.updatePlayerAsteroidCollision player as holder {hAsteroids =  a : (hAsteroids holder)} 
                            where playerLocation = (plLocation (hPlayer holder))
                                  updateCollidedPlayer player = player { plLocation = (0,0), plSpeed = (0,0), lives = (lives player) - 1} 



-- |  split   Asteroid to    tow  part
breakAsteroid :: Asteroid  -- ^ Asteroid want  split
 ->Float -- ^ count  of part
 ->[Asteroid] -- ^ list  of new Asteroids 
breakAsteroid  asteroid count 
  |count == 0 ||  (size asteroid)  == 0 =[ ]
breakAsteroid asteroid  count=asteroid{
  size = (size asteroid) -1
  , aLocation = ( aLocation asteroid )
  , aSpeed = (rand (count * thrustMaxSpeed * 0.5), rand (count * thrustMaxSpeed * 0.5))  
  , radius = (radius asteroid) / 2
} : breakAsteroid asteroid  (count -1)

--distance ast = sqrt (( fst (prLocation projectile) - fst (aLocation ast))^2 + ( snd (prLocation projectile) - snd (aLocation ast))^2)
                                                -- 

--------Events Hndling
-- | Handle the player keys, like thrust rotate, fire, etc
handleGeneralKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleGeneralKeys (EventKey (Char 'd') Down _ _) game = game { players = updateRotationStates (players game) (-rotationSpeed) True 1}    -- Rotate the ship Clock-Wise when press 'd'
handleGeneralKeys (EventKey (Char 'd') Up _ _) game = game { players = updateRotationStates (players game) (-rotationSpeed) False 1}
handleGeneralKeys (EventKey (Char 'a') Down _ _) game = game { players = updateRotationStates (players game) (rotationSpeed) True 1}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleGeneralKeys (EventKey (Char 'a') Up _ _) game = game { players = updateRotationStates (players game) (rotationSpeed) False 1}
handleGeneralKeys (EventKey (Char 'p') Down _ _) game = game {gameMode = Pause}   -- Pause the game when press 'p'
handleGeneralKeys (EventKey (Char 'q') Down _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'
handleGeneralKeys (EventKey (Char 'w') Down _ _) game = game {players = updateThrustStatus (players game) True 1}
handleGeneralKeys (EventKey (Char 'w') Up _ _) game = game {players = updateThrustStatus (players game) False 1}
handleGeneralKeys (EventResize (w,h)) game = game {gWidth = (fromIntegral w) , gHeight = (fromIntegral h)}
handleGeneralKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game {players = updateFireStatus (players game) True 1}
handleGeneralKeys (EventKey (SpecialKey KeySpace) Up _ _) game = game {players = updateFireStatus (players game) False 1}
handleGeneralKeys event game
                            | (gameMode game) == Single = handleSingleplayerKeys event game
                            | (gameMode game) == Cooperative = handleCooperativeKeys event game 
handleGeneralKeys _ game = game

--hazem add key event on spacebar to fire 
-- u may use this reference: https://hackage.haskell.org/package/gloss-1.11.1.1/docs/Graphics-Gloss-Interface-IO-Game.html

-- Display the basic contents of the general game like the stars, asteroids, etc
generalRender :: AsteroidsGame -> Picture
generalRender game 
  |(gameMode game) == Single = 
  pictures
   ([
      --assume this value is the number of stars
      mkStars 111
   ]
   ++
   [
      renderAsteroid game
   ]
   ++
   [
      spRender game
   ])

  |(gameMode game) == Cooperative = 
  pictures
   ([
      --assume this value is the number of stars
      mkStars 111
   ]
   ++
   [
      renderAsteroid game
   ]
   ++
   [
      coRender game
   ])

   where
    -- | Display a random stars
    mkStars 
     :: Int -- ^ Number of the stars
     -> Picture
    mkStars n = pictures
     [
       translate (fst l) (snd l) $ color blue (circleSolid 2) | l <- getVal (randX n) (randY n)
     ]

    -- | Get a random numbers for the stars x-axis
    randX :: Int -> [Float]
    randX n = take n (randomRs ((-(gWidth game)), (gWidth game) :: Float) (mkStdGen n))
    -- | Get a random numbers for the stars y-axis
    randY :: Int -> [Float]
    randY n = take n (randomRs ((-(gHeight game)), (gHeight game) :: Float) (mkStdGen (n*2)))

    -- | Merge the random nubmers of the stars x-axis and y-axis in a tuple
    getVal :: [Float] -> [Float] -> [(Float,Float)] 
    getVal [] [] = []
    getVal [] _ = []
    getVal _ [] = []
    getVal (x:xs) (y:ys) = (x,y) : (getVal xs ys)