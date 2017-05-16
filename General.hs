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

----------Game Updates
updateGeneralGame :: AsteroidsGame -> AsteroidsGame 
updateGeneralGame  = projectilesCollision.updateGameBasedOnMode
                             


updateGameBasedOnMode game  | (gameMode game) == Single = SinglePlayer.updateSinglePlayerGame game
                            | (gameMode game) == Cooperative = Cooperative.updateCooperativeGame game


projectilesCollision :: AsteroidsGame -> AsteroidsGame
projectilesCollision game = updatePlayerCollision (players game) $ game {players = []}
                         

updatePlayerCollision :: [Player] -> AsteroidsGame -> AsteroidsGame
updatePlayerCollision [] game = game
updatePlayerCollision (p:ps) game = updatePlayerCollision ps $ game { players = bindPlayers, asteroids = (hAsteroids calcCollision)}
                                     where calcCollision = projectilesCollisionHelper (projectiles (p)) Holder{hProjectiles = [], hAsteroids = (asteroids game)}
                                           bindPlayers = p {projectiles = (hProjectiles calcCollision)} : (players game)
bindPlayers :: Player -> [Player] -> [Player]
bindPlayers p ps = p : ps

projectilesCollisionHelper ::  [Projectile] -> Holder  -> Holder
projectilesCollisionHelper [] holder = holder
projectilesCollisionHelper  (pr:prs) holder = projectilesCollisionHelper prs $ holder {hProjectiles = (hProjectiles holder) ++ (hProjectiles prCollision), hAsteroids = (hAsteroids prCollision)}
                                              where prCollision =  projectilesCollisionHelper2 Holder{hProjectiles = [], hAsteroids = []} pr (hAsteroids holder) False

projectilesCollisionHelper2 :: Holder -> Projectile -> [Asteroid] -> Bool -> Holder
projectilesCollisionHelper2 holder _ [] True = holder
projectilesCollisionHelper2 holder projectile [] False = holder{ hProjectiles = projectile : (hProjectiles holder)}
projectilesCollisionHelper2 holder projectile (a:as) collided
                                                    | distance a < (radius a) = projectilesCollisionHelper2 holder {hAsteroids =  (hAsteroids holder) ++ breakeAsteroid a 2}  projectile as True
                                                    | otherwise = projectilesCollisionHelper2 holder {hAsteroids =  a : (hAsteroids holder)}  projectile as collided
                            where distance ast = sqrt (( fst (prLocation projectile) - fst (aLocation ast))^2 + ( snd (prLocation projectile) - snd (aLocation ast))^2)





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