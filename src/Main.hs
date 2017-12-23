module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Render
import State
import Logic

window :: Display
window = InWindow "Pong" (300, 300) (-1, -1)

background :: Color
background = white

fps :: Int
fps = 60

update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = wallBounce . paddleBounce . moveBall seconds

main :: IO ()
main = simulate window background fps initialState render update
