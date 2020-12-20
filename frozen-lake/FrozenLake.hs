module FrozenLake
  ( FrozenLake
  , Action
  , GameState
  , makeGame
  , makeMove
  , getGameState
  )
  where

import           Control.Monad.State
import           Data.Random         (sampleState, uniform)
import           System.Random       (StdGen, mkStdGen)


-- | The frozen lake game instance. We keep the gameGen inside so that we can
-- use the state monad whenever random moves need to be performed on the game.
-- This way we can escape the IO monad and keep the code pure.
data FrozenLake = FrozenLake
  { dims           :: (Int, Int)
  , playerPosition :: (Int, Int)
  , goalPosition   :: (Int, Int)
  , holePositions  :: [(Int, Int)]
  , gameGen        :: StdGen } deriving Show

data Action = U | D | L | R deriving (Eq, Show)

data GameState = OnGoing | Won | Lost deriving (Eq, Show)

getGameState :: FrozenLake -> GameState
getGameState l
  | playerPosition l `elem` holePositions l = Lost
  | playerPosition l == goalPosition l      = Won
  | otherwise                               = OnGoing

-- | This function receives an action and moves the player. Because the ice is
--   slippery the player makes, with probability 0.1 may move one extra square
--   in the chosen direction.
makeMove :: Action -> State FrozenLake ()
makeMove a = do
  game <- get
  let (x, y)        = playerPosition game
      (val, newGen) = sampleState (uniform 0 1) (gameGen game) :: (Float, StdGen)
      (n, m)        = dims game
  put $ case a of
          U -> game { playerPosition = (x, if val >= 0.1 then maximum [0,y-1] else maximum [0,y-2])
                    , gameGen        = newGen}
          D -> game { playerPosition = (x, if val >= 0.1 then minimum [m,y+1] else maximum [m,y+2])
                    , gameGen        = newGen}
          L -> game { playerPosition = (if val >= 0.1 then maximum [0,x-1] else maximum [0,x-2], y)
                    , gameGen        = newGen}
          R -> game { playerPosition = (if val >= 0.1 then minimum [n,x+1] else minimum [n,x+2], y)
                    , gameGen        = newGen}

emptyLake :: (Int, Int) -> FrozenLake
emptyLake d =
  FrozenLake { dims = d
             , playerPosition = (0,0)
             , goalPosition = (0, 0)
             , holePositions = []
             , gameGen = mkStdGen 1
             }

-- | Input:
--      * The dimensions of the board
--      * The number of holes
--      * A seed to generate the random board
--   Output: An instance of the frozen lake game
makeGame :: (Int, Int) -> Int -> StdGen -> FrozenLake
makeGame size n g = execState (setPlayer >> setGoal >> setHoles n)
                              ((emptyLake size) {gameGen = g})

setHoles :: Int -> -- The number of holes to place
            State FrozenLake () -- the new game state
setHoles n = forM_ [1..n] (\_ -> setHole)

setHole :: State FrozenLake ()
setHole = do
  game <- get
  let (x, g)   = sampleState (uniform 0 (fst $ dims game)) (gameGen game)
      (y, g')  = sampleState (uniform 0 (snd $ dims game)) g
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
