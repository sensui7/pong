{-
File describes the game functions and logic.
To-Do: correct the logic how the ball gets reflected instead 
of simply changing the x velocity vector
-}

module Logic(accelerateURight, accelerateDRight, paddleAI, checkWinner, moveBall, wallBounce, paddleBounce, pauseGame, handleKeys) where

import State
import Graphics.Gloss.Interface.Pure.Game

-- | Type-naming for clarity in the below functions
type Radius = Float
type Location = Float
type Position = (Float, Float)

-- | Move the ball by updating old velocity to new velocity
moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
   where
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      x' = x + vx * seconds
      y' = y + vy * seconds

-- | Use the game state values to check for wall collisions
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
   where
      topCollision = y - radius <= -fromIntegral 300 / 2
      bottomCollision = y + radius >= fromIntegral 300 / 2

-- | Change the ball velocity direction based on collision values
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
   where
      radius = 10
      (vx, vy) = ballVel game
      
      vy' = if wallCollision (ballLoc game) radius
            then -vy else vy

-- | Check who's won and reset the ball to the original position with slower speed
checkWinner :: PongGame -> PongGame
checkWinner game = game { ballVel = (vx', vy'), ballLoc = (x', y') }
   where
     (vx, vy) = ballVel game
     (x, y) = ballLoc game
     radius = 10

     (x', y') = if x - 10 > 150 || x + 10 < (-150) then (0, 0) else (x, y)
     (vx', vy') = if x' == 0 && y' == 0 then (150, 30) else (vx, vy)

-- | Check if the ball touches the right paddle (calculated by length ranges)
paddleCollisionRight :: Position -> Radius -> Location -> (Bool, Bool)
paddleCollisionRight (x,y) radius paddleRight = (rightUp, rightDown)
   where
      (rightUp, rightDown) = (x - radius >= 90 && y <= (paddleRight + 43) && y >= paddleRight,
                              x - radius >= 90 && y <= paddleRight && y >= (paddleRight - 43))

-- | Check if the ball touches the left paddle (calculated by length ranges)
paddleCollisionLeft :: Position -> Radius -> Location -> (Bool, Bool)
paddleCollisionLeft (x,y) radius paddleLeft = (leftUp, leftDown)
   where
      (leftUp, leftDown) = (x + radius <= (-90) && y <= (paddleLeft + 43) && y >= paddleLeft,
                              x + radius <= (-90) && y <= paddleLeft && y >= (paddleLeft - 43))

-- | Change the ball direction after checking for paddle collision
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy'), player1 = paddleRight, player2 = paddleLeft }
   where
      radius = 10
      (vx, vy) = ballVel game
      paddleLeft = player2 game
      paddleRight = player1 game

      -- Handle the logic for paddle collisions using ball velocity vectors and boolean-based collision
      (vx', vy') = if (paddleCollisionRight (ballLoc game) radius paddleRight) == (True, False) && vx >= 0 && vy >= 0
                   then (-vx, vy + 10)
                   else if (paddleCollisionRight (ballLoc game) radius paddleRight) == (False, True) && vx >= 0 && vy >= 0
                   then (-vx, -vy + 15)
                   else if (paddleCollisionRight (ballLoc game) radius paddleRight) == (True, False) && vx >= 0 && vy <= 0
                   then (-vx, -vy + 15)
                   else if (paddleCollisionRight (ballLoc game) radius paddleRight) == (False, True) && vx >= 0 && vy <= 0
                   then (-vx, vy + 10)
                   else if (paddleCollisionLeft (ballLoc game) radius paddleLeft) == (True, False) && vx <= 0 && vy <= 0
                   then (-vx + 10, -vy)
                   else if (paddleCollisionLeft (ballLoc game) radius paddleLeft) == (False, True) && vx <= 0 && vy >= 0
                   then (-vx + 15, -vy)
                   else if (paddleCollisionLeft (ballLoc game) radius paddleLeft) == (True, False) && vx <= 0 && vy >= 0
                   then (-vx + 15, vy)
                   else if (paddleCollisionLeft (ballLoc game) radius paddleLeft) == (False, True) && vx <= 0 && vy <= 0
                   then (-vx + 10, vy)
                   else (vx, vy)

-- | Hardcore AI that's not "unbeatable"
paddleAI :: PongGame -> PongGame
paddleAI game = game { ballVel = (vx, vy), ballLoc = (x, y), player2 = paddleLeft' }
    where
      paddleLeft = player2 game
      (vx, vy) = ballVel game
      (x, y) = ballLoc game

      -- Move fast towards the ball using y coordinate estimates
      paddleLeft' = if vx <= 0 && vy > 0 && (paddleLeft <= 100) && paddleLeft < y
                    then paddleLeft + 2
                    else if vx <= 0 && vy < 0 && (paddleLeft >= -100) && paddleLeft > y
                    then paddleLeft - 2
                    else if paddleLeft >= 100 && y <= paddleLeft + 43 && y >= paddleLeft - 43
                    then paddleLeft - 0.2
                    else if paddleLeft <= (-100) && y >= paddleLeft + 43 && y >= paddleLeft - 43 
                    then paddleLeft + 0.5
                    else paddleLeft

-- Pause the game by changing one of the data structure values
-- Handled by update in Main.hs
pauseGame :: PongGame -> Bool
pauseGame game | pause game == False = False | otherwise = True

moveUpLeft :: PongGame -> PongGame
moveUpLeft game = game { player2 = paddleLeft' }
   where
      paddleLeft = player2 game
      paddleLeft' = if (paddleLeft <= 100 && paddleLeft >= (-100)) then paddleLeft + 5 else paddleLeft - 5

moveDownLeft :: PongGame -> PongGame
moveDownLeft game = game { player2 = paddleLeft' }
   where
      paddleLeft = player2 game
      paddleLeft' = if (paddleLeft <= 100 && paddleLeft >= (-100)) then paddleLeft - 5 else paddleLeft + 5

moveUpRight :: PongGame -> PongGame
moveUpRight game = game { player1 = paddleRight'}
   where
      paddleRight = player1 game
      paddleRight' = if (paddleRight <= 100) then paddleRight + 5 else paddleRight - 5

moveDownRight :: PongGame -> PongGame
moveDownRight game = game { player1 = paddleRight' }
   where
      paddleRight = player1 game
      paddleRight' = if (paddleRight >= (-100)) then paddleRight - 5 else paddleRight + 5

-- | Accelerate up for right player
accelerateURight :: PongGame -> PongGame
accelerateURight game = game { player1 = paddleRight'}
   where
      paddleRight = player1 game
      accel = accelerateUp game

      paddleRight' = if accel == True && paddleRight <= 100 && paddleRight >= (-100) then paddleRight + 2
                     else if accel == False && paddleRight >= 100 && paddleRight <= (-100) then paddleRight - 5
                     else paddleRight 

-- | Accelerate down for right player
accelerateDRight :: PongGame -> PongGame
accelerateDRight game = game { player1 = paddleRight'}
   where
      paddleRight = player1 game
      accel = accelerateDown game

      paddleRight' = if accel == True && paddleRight >= (-100) && paddleRight <= 100 then paddleRight - 2
                     else if accel == False && paddleRight <= (-100) then paddleRight + 5
                     else paddleRight 

-- I/O for gameplay
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'w') _ _ _) game = moveUpLeft game
handleKeys (EventKey (Char 's') _ _ _) game = moveDownLeft game
handleKeys (EventKey (Char 'i') Down _ _) game = moveUpRight game {accelerateUp = True}
handleKeys (EventKey (Char 'i') Up _ _) game = game {accelerateUp = False}
handleKeys (EventKey (Char 'k') Down _ _) game = moveDownRight game {accelerateDown = True}
handleKeys (EventKey (Char 'k') Up _ _) game = game {accelerateDown = False}
handleKeys (EventKey (Char 'p') _ _ _) game = game {pause = True}
handleKeys (EventKey (Char 'o') _ _ _) game = game {pause = False}
handleKeys (EventKey (Char 'r') _ _ _) game = game {ballLoc = (0, 0)}
handleKeys _ game = game
