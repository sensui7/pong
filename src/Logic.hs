module Logic(moveBall, wallBounce, paddleBounce) where

import State

type Radius = Float
type Location = Float
type Position = (Float, Float)

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
   where
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      x' = x + vx * seconds
      y' = y + vy * seconds

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
   where
      topCollision = y - radius <= -fromIntegral 300 / 2
      bottomCollision = y + radius >= fromIntegral 300 / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
   where
      radius = 10
      (vx, vy) = ballVel game
      
      vy' = if wallCollision (ballLoc game) radius
            then -vy else vy

paddleCollision :: Position -> Radius -> Location -> Location -> Bool
paddleCollision (x,y) radius paddleRight paddleLeft = rightCollision || leftCollision
   where
      rightCollision = x - radius >= 90 && y <= (paddleRight + 43) && y >= (paddleRight - 43)
      leftCollision = x + radius <= (-90) && y <= (paddleLeft + 43) && y >= (paddleLeft - 43)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy'), player1 = paddleRight, player2 = paddleLeft'}
   where
      radius = 10
      (vx, vy) = ballVel game
      paddleLeft' = 0
      paddleRight = player1 game

      (vx', vy') = if paddleCollision (ballLoc game) radius paddleRight paddleLeft'
                   then (-vx, -vy)
                   else (vx, vy)

      --vx' = if paddleCollision (ballLoc game) radius paddleLoc then -vx else vx
      --vy' = if paddleCollision (ballLoc game) radius paddleLoc then -vy else vy







