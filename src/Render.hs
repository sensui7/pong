{-
Module provides drawing the initial state of the game, colors of the objects,
and sizes of the objects.
-}

module Render(render) where

import Graphics.Gloss
import State

render :: PongGame -> Picture
render game = pictures [ball, walls, mkPaddle red 120 $ player1 game, mkPaddle orange (-120) $ player2 game]
   where
     -- Create the ball of radius 10
     ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
     ballColor = green

     -- Create the top and bottom walls
     wall :: Float -> Picture
     wall offset =
        translate 0 offset $ color wallColor $ rectangleSolid 300 10

     -- Personalize the wall colors
     wallColor = white
     walls = pictures[wall 150, wall (-150)]

     -- Personalize and set up the player paddles
     mkPaddle :: Color -> Float -> Float -> Picture
     mkPaddle col x y = pictures
        [
         translate x y $ color col $ rectangleSolid 26 86, 
         translate x y $ color paddleColor $ rectangleSolid 20 80
        ]

     paddleColor = white
