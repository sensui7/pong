-- File describes the data structure required for gameplay
-- and the initial state of the game

module State(PongGame(..), initialState) where

-- | Data structure for the pong game
data PongGame = Game
   { ballLoc :: (Float, Float), -- ^ Pong ball (x, y) location
     ballVel :: (Float, Float), -- ^ Pong ball (x, y) velocity
     player1 :: Float,          -- ^ Right player
     player2 :: Float,          -- ^ Left player
     accelerateUp :: Bool,      -- ^ Right player accelerate up
     accelerateDown :: Bool,    -- ^ Right player accelerate down
     pause   :: Bool            -- ^ Check for pause state
   } deriving Show

-- | How the game starts
initialState :: PongGame
initialState = Game
   { ballLoc = (-10, 30),
     ballVel = (150, 150),
     player1 = 40,
     player2 = 40,
     accelerateUp = False,
     accelerateDown = False,
     pause = False
   }
