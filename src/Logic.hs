module Logic(moveBall, wallBounce, paddleBounce) where

import State

type Radius = Float
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

paddleCollision :: Position -> Radius -> Float -> Bool
paddleCollision (x,y) radius paddle = rightCollision || leftCollision
   where
      rightCollision = x - 10 >= 90 && y <= (paddle + 43) && y >= (paddle - 43)
      leftCollision = x + 10 <= -90 && y <= (paddle + 43) && y >= (paddle - 43)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy'), player1 = m', player2 = n'}
   where
      radius = 10
      (vx, vy) = ballVel game
      m' = -10
      n' = 0

      vx' = if paddleCollision (ballLoc game) radius m' then -vx else vx
      vy' = if paddleCollision (ballLoc game) radius m' then -vy else vy










