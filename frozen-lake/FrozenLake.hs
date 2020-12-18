module FrozenLake
  (FrozenLake)
  where

import           Control.Monad.State
import           Data.Random         (normal, sampleState, uniform)
import           System.Random       (StdGen)

-- | tiles are: Starting Point, Frozen, Hole, Goal
data Tile = S | F | H | G deriving (Eq, Show)

-- | The frozen lake game instance. We keep the gameGen inside so that we can
-- use the state monad whenever random moves need to be performed on the game.
-- This way we can escape the IO monad and keep the code pure.
data FrozenLake = FrozenLake
  { dims           :: (Int, Int)
  , playerPosition :: (Int, Int)
  , goalPosition   :: (Int, Int)
  , holePositions  :: [(Int, Int)]
  , gameGen        :: StdGen } deriving Show

emptyLake :: FrozenLake
emptyLake = FrozenLake { holePositions = [] }
-- | Input:
--      * The dimensions of the board
--      * The number of holes
--      * A seed to generate the random board
--   Output: An instance of the frozen lake game
makeGame :: (Int, Int) -> Int -> StdGen -> FrozenLake
makeGame size n g = execState (setPlayer >> setGoal >> setHoles n)
                              (emptyLake {gameGen = g, dims = size})

setHoles :: Int -> -- The number of holes to place
            State FrozenLake () -- the new game state
setHoles n = forM_ [1..n] (\_ -> setHole)

setHole :: State FrozenLake ()
setHole = do
  game <- get
  let (x, g)  = sampleState (uniform 0 (fst $ dims game)) (gameGen game)
      (y, g') = sampleState (uniform 0 (snd $ dims game)) g
      newHoles = [(x, y)] ++ holePositions game
  put $ game {holePositions = newHoles, gameGen = g'}

setGoal :: State FrozenLake ()
setGoal = do
  game <- get
  let (x, g)  = sampleState (uniform 0 (fst $ dims game)) (gameGen game)
      (y, g') = sampleState (uniform 0 (snd $ dims game)) g
  put $ game {goalPosition = (x, y), gameGen = g'}

setPlayer :: State FrozenLake ()
setPlayer = do
  game <- get
  let (x, g)  = sampleState (uniform 0 (fst $ dims game)) (gameGen game)
      (y, g') = sampleState (uniform 0 (snd $ dims game)) g
  put $ game {playerPosition = (x, y), gameGen = g'}
