import           Control.Monad.State
import           Data.Random         (sampleState, uniform)
import           FrozenLake
import           System.Random       (StdGen, mkStdGen)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Ord
import Data.List

data Carlo s = Carlo
  { policy  :: M.Map s Action
  , qValues :: M.Map (s, Action) Float
  , states  :: [s]
  , actions :: [Action]
  , epsilon :: Float
  , gen     :: StdGen
  } deriving Show

type AgentState = (Int, Int)

-- | Simply creates an agent from a game
agentFromGame :: FrozenLake -> Float -> Carlo AgentState
agentFromGame game eps=
  Carlo { policy  = M.fromList [(s, U) | s <- tiles]
        , qValues = M.fromList [((s, a), 0) | s <- tiles, a <- [U, D, L, R, KeepStill]]
        , states  = tiles
        , actions = [U, D, L, R, KeepStill]
        , epsilon = eps
        , gen     = mkStdGen 1}
  where tiles = getTileList game

-- | Input: Initial game, Initial agent, number of moves
--   Output: An episode in the from state action reward
episode :: FrozenLake -> Carlo AgentState -> Int -> [(AgentState, Action, Float)]
episode game agent n = evalState (forM [1..n] (\_ -> agentTurn)) (game, agent)

-- | The resulting state of the agent/game after taking one action
-- Notice that here we already use Carlo (Int, Int) because we must
-- have access to a concrete state.
-- (AgentState, Action, Float) = (PreviouState, PreviousAction, RewardAfterAction)
agentTurn :: State (FrozenLake, Carlo AgentState) (AgentState, Action, Float)
agentTurn = do
  (game, agent) <- get
  let (action, newAgent) = runState (takeAction $ playerPosition game) agent
      newGame            = execState (makeMove action) game
      reward             = getReward newGame
  put (newGame, newAgent)
  return (playerPosition game, action, reward)

-- | Input: A state
--   Output: An action and an agent in the state monad. The
--   action is taken from the policy of the agent with probability epsilon that
--   it is a random action
takeAction :: Ord s => s -> State (Carlo s) Action
takeAction s = do
  agent <- get
  let (val, g) = sampleState ( uniform 0 1) (gen agent)
      action   = if val >= epsilon agent
                 then fromJust $ policy agent M.!? s
                 else choice (actions agent) g
  put agent { gen = g }
  return action

-- | This function updates the policy of the agent based on the current qValues
-- in a greedy way. Meaning that, after the update, the agent simply chooses in
-- state s the action that had the largest (s, action) value
makeGreedy :: (Ord s) => Carlo s -> Carlo s
makeGreedy agent =
  agent { policy = M.fromList [(s, getMaximumAction agent s) | s <- states agent]}

-- | For a given state returns the agent with largest Q value
getMaximumAction :: (Ord s) => Carlo s -> s -> Action
getMaximumAction agent st =
  maximumBy (comparing f) (actions agent)
  where f a = fromMaybe 0 $ qValues agent M.!? (st, a)

-- | Chooses a random element from a list
choice :: [a] -> StdGen -> a
choice l g = l!!(fst $ sampleState (uniform 0 (-1 + length l)) g)

main :: IO()
main = undefined
