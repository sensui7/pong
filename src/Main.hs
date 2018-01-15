-- Module deals with the interface using gloss

module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Render
import State
import Logic

window :: Display
window = InWindow "Pong" (300, 300) (500, 0)

background :: Color
background = white

fps :: Int
fps = 60

-- Old update for simulate and simple runtime
--update :: ViewPort -> Float -> PongGame -> PongGame
--update _ seconds = wallBounce . paddleBounce . moveBall seconds
--update seconds game = wallBounce . paddleBounce . moveBall seconds

update :: Float -> PongGame -> PongGame
update seconds game 
    | pauseGame game == True = game 
    | otherwise = accelerateDRight $ accelerateURight $ 
                  paddleAI $ checkWinner $ wallBounce $ paddleBounce $ moveBall seconds game

main :: IO ()
main = play window background fps initialState render handleKeys update
