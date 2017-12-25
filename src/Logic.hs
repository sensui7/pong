{-
File describes the game functions and logic.
To-Do: correct the logic how the ball gets reflected instead 
of simply changing the x velocity vector
-}

module Logic(moveBall, wallBounce, paddleBounce, pauseGame, handleKeys) where

import State
import Graphics.Gloss.Interface.Pure.Game

-- Type-naming for clarity in the below functions
type Radius = Float
type Location = Float
type Position = (Float, Float)

-- Move the ball by updating old velocity to new velocity
moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
   where
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      x' = x + vx * seconds
      y' = y + vy * seconds

-- Use the game state values to check for wall collisions
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
   where
      topCollision = y - radius <= -fromIntegral 300 / 2
      bottomCollision = y + radius >= fromIntegral 300 / 2

-- Change the ball velocity direction based on collision values
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
   where
      radius = 10
      (vx, vy) = ballVel game
      
      vy' = if wallCollision (ballLoc game) radius
            then -vy else vy

-- Check if the ball touches the paddle (calculated by length ranges)
paddleCollision :: Position -> Radius -> Location -> Location -> Bool
paddleCollision (x,y) radius paddleRight paddleLeft = rightCollision || leftCollision
   where
      rightCollision = x - radius >= 90 && y <= (paddleRight + 43) && y >= (paddleRight - 43)
      leftCollision = x + radius <= (-90) && y <= (paddleLeft + 43) && y >= (paddleLeft - 43)

-- Change the ball direction after checking for paddle collision
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy'), player1 = paddleRight, player2 = paddleLeft}
   where
      radius = 10
      (vx, vy) = ballVel game
      paddleLeft = player2 game
      paddleRight = player1 game

      (vx', vy') = if paddleCollision (ballLoc game) radius paddleRight paddleLeft
                   then (-vx, vy)
                   else (vx, vy)

-- Pause the game by changing one of the data structure values
-- Handled by update in Main.hs
pauseGame :: PongGame -> Bool
pauseGame game | pause game == False = False | otherwise = True

moveUpLeft :: PongGame -> PongGame
moveUpLeft game = game { player2 = paddleLeft' }
   where
      paddleLeft = player2 game
      paddleLeft' = if (paddleLeft <= 100 && paddleLeft >= -100) then paddleLeft + 5 else paddleLeft - 5

moveDownLeft :: PongGame -> PongGame
moveDownLeft game = game { player2 = paddleLeft' }
   where
      paddleLeft = player2 game
      paddleLeft' = if (paddleLeft <= 100 && paddleLeft >= -100) then paddleLeft - 5 else paddleLeft + 5

moveUpRight :: PongGame -> PongGame
moveUpRight game = game { player1 = paddleRight' }
   where
      paddleRight = player1 game
      paddleRight' = if (paddleRight <= 100 && paddleRight >= -100) then paddleRight + 5 else paddleRight - 5

moveDownRight :: PongGame -> PongGame
moveDownRight game = game { player1 = paddleRight' }
   where
      paddleRight = player1 game
      paddleRight' = if (paddleRight <= 100 && paddleRight >= -100) then paddleRight - 5 else paddleRight + 5

-- I/O for gameplay
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'w') _ _ _) game = moveUpLeft game
handleKeys (EventKey (Char 's') _ _ _) game = moveDownLeft game
handleKeys (EventKey (Char 'i') _ _ _) game = moveUpRight game
handleKeys (EventKey (Char 'k') _ _ _) game = moveDownRight game
handleKeys (EventKey (Char 'p') _ _ _) game = game {pause = True}
handleKeys (EventKey (Char 'o') _ _ _) game = game {pause = False}
handleKeys (EventKey (Char 'r') _ _ _) game = game {ballLoc = (0, 0)}
handleKeys _ game = game
