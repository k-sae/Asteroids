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
updateGeneralGame :: AsteroidsGame -> AsteroidsGame 
updateGeneralGame  = projectilesCollision.updateGameBasedOnMode
                             


updateGameBasedOnMode game  | (gameMode game) == Single = SinglePlayer.updateSinglePlayerGame game
                            | (gameMode game) == Cooperative = Cooperative.updateCooperativeGame game


projectilesCollision :: AsteroidsGame -> AsteroidsGame
projectilesCollision game = updatePlayerCollision (players game) $ game {players = []}
                         

updatePlayerCollision :: [Player] -> AsteroidsGame -> AsteroidsGame
updatePlayerCollision [] game = game
updatePlayerCollision (p:ps) game = updatePlayerCollision ps $ (checkDeadPlayers game (p:ps)) { asteroids = (hAsteroids plAstCollision), players = bindPlayers}
                                     where prAstCollision = projectilesCollisionHelper (projectiles (p)) Holder{hProjectiles = [], hAsteroids = (asteroids game), noOfCollision = 0, hPlayer = p}
                                           bindPlayers = (players game) ++ [(hPlayer plAstCollision)]
                                           plAstCollision = General.updatePlayerAsteroidCollision updateP  (hAsteroids prAstCollision) Holder {hProjectiles = [], hAsteroids = [], noOfCollision = 0, hPlayer = updateP} 
                                           updateP = p {projectiles = (hProjectiles prAstCollision), score = (noOfCollision prAstCollision)*10 + (score p), lives = (lives (hPlayer prAstCollision))}

deadPlayers :: AsteroidsGame -> [Player] -> [Player]
deadPlayers game players = [ player | player <- players, (lives player) <=0] 

checkDeadPlayers :: AsteroidsGame -> [Player]-> AsteroidsGame
checkDeadPlayers game players
 | dead == [] = game
 | (gameMode game) == Single =  game{gameMode = GameOver}
 | (gameMode game) == Cooperative = cooperativeDeadPlayers game dead
 | otherwise = game
 where
  dead = (deadPlayers game players)

cooperativeDeadPlayers :: AsteroidsGame -> [Player] -> AsteroidsGame
--cooperativeDeadPlayers game [x] = game{players = []} -- if one player is dead and the another is not
cooperativeDeadPlayers game players = game{gameMode = GameOver}

projectilesCollisionHelper ::  [Projectile] -> Holder  -> Holder
projectilesCollisionHelper [] holder = holder
projectilesCollisionHelper  (pr:prs) holder = projectilesCollisionHelper prs $ holder {hProjectiles = (hProjectiles holder) ++ (hProjectiles prCollision), hAsteroids = (hAsteroids prCollision), noOfCollision = (noOfCollision holder) + (noOfCollision prCollision)}
                                              where prCollision =  projectilesCollisionHelper2 Holder{hProjectiles = [], hAsteroids = [], noOfCollision = 0, hPlayer = (hPlayer holder)} pr (hAsteroids holder) False

projectilesCollisionHelper2 :: Holder -> Projectile -> [Asteroid] -> Bool -> Holder
projectilesCollisionHelper2 holder _ [] True = holder
projectilesCollisionHelper2 holder projectile [] False = holder{ hProjectiles = projectile : (hProjectiles holder)}
-- 2 possible issues up here
--update: i have choosen to ignore them as the fault value will neglictable (nearly 2px)
projectilesCollisionHelper2 holder projectile (a:as) collided
                                                    | distance (aLocation a) (prLocation projectile) <= (radius a) = projectilesCollisionHelper2 holder {hAsteroids =  (hAsteroids holder) ++ breakAsteroid a 2, noOfCollision = (noOfCollision holder) + 1}  projectile as True
                                                    | otherwise = projectilesCollisionHelper2 holder {hAsteroids =  a : (hAsteroids holder)}  projectile as collided

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1,y1) (x2,y2) =  sqrt ((x1 - x2)^2 + (y1 - y2)^2)
updatePlayerAsteroidCollision:: Player -> [Asteroid] -> Holder -> Holder
updatePlayerAsteroidCollision _ [] holder = holder
updatePlayerAsteroidCollision player (a:as) holder 
                                                  | distance (aLocation a) playerLocation <= (radius a) = General.updatePlayerAsteroidCollision player as holder { hPlayer = updateCollidedPlayer (hPlayer holder)}  
                                                  | otherwise =   General.updatePlayerAsteroidCollision player as holder {hAsteroids =  a : (hAsteroids holder)} 
                            where playerLocation = (plLocation (hPlayer holder))
                                  updateCollidedPlayer player = player { plLocation = (0,0), plSpeed = (0,0), lives = (lives player) - 1} 




breakAsteroid :: Asteroid ->Float->[Asteroid] 
breakAsteroid  asteroid count 
  |count == 0 ||  (size asteroid)  == 0 =[ ]
breakAsteroid asteroid  count=asteroid{
  size = (size asteroid) -1
  , aLocation = ( aLocation asteroid )
  , aSpeed = (rand (count * 3), rand (count * 3))  
  , radius = (radius asteroid) / 2
} : breakAsteroid asteroid  (count -1)

--distance ast = sqrt (( fst (prLocation projectile) - fst (aLocation ast))^2 + ( snd (prLocation projectile) - snd (aLocation ast))^2)
                                                -- 

--------Events Hndling
handleGeneralKeys :: Event -> AsteroidsGame -> AsteroidsGame
handleGeneralKeys (EventKey (Char 'd') Down _ _) game = game { players = updateRotationStates (-rotationSpeed) True (players game) 0}    -- Rotate the ship Clock-Wise when press 'd'
handleGeneralKeys (EventKey (Char 'd') Up _ _) game = game { players = updateRotationStates (-rotationSpeed) False (players game) 0}
handleGeneralKeys (EventKey (Char 'a') Down _ _) game = game { players = updateRotationStates (rotationSpeed) True (players game) 0}   -- Rotate the ship Anti_Clock-Wise when press 'a'
handleGeneralKeys (EventKey (Char 'a') Up _ _) game = game { players = updateRotationStates (rotationSpeed) False (players game) 0}
handleGeneralKeys (EventKey (Char 'p') Down _ _) game = game {gameMode = Pause}   -- Pause the game when press 'p'
handleGeneralKeys (EventKey (Char 'q') Down _ _) game = game {gameMode = Menu}   -- Return to the menu and quit the game when press 'q'
handleGeneralKeys (EventKey (Char 'w') Down _ _) game = game {players = updateThrustStatus (players game) True 0 0}
handleGeneralKeys (EventKey (Char 'w') Up _ _) game = game {players = updateThrustStatus (players game) False 0 0}
handleGeneralKeys (EventResize (w,h)) game = game {gWidth = (fromIntegral w) , gHeight = (fromIntegral h)}
handleGeneralKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game {players = updateFireStatus (players game) True 0 0}
handleGeneralKeys (EventKey (SpecialKey KeySpace) Up _ _) game = game {players = updateFireStatus (players game) False 0 0}
handleGeneralKeys event game
                            | (gameMode game) == Single = handleSingleplayerKeys event game
                            | (gameMode game) == Cooperative = handleCooperativeKeys event game 
handleGeneralKeys _ game = game

--hazem add key event on spacebar to fire 
-- u may use this reference: https://hackage.haskell.org/package/gloss-1.11.1.1/docs/Graphics-Gloss-Interface-IO-Game.html

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
    mkStars :: Int -> Picture
    mkStars n = pictures
     [
       translate (fst l) (snd l) $ color blue (circleSolid 2) | l <- getVal (randX n) (randY n)
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