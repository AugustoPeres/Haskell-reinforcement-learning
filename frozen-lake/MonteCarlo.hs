import           Control.Monad.State
import           Data.List
import qualified Data.Map.Lazy       as M
import           Data.Maybe
import           Data.Ord
import           Data.Random         (sampleState, uniform)
import           FrozenLake
import           System.Random       (StdGen, mkStdGen)

data Carlo s = Carlo
  { policy  :: M.Map s Action
  , qValues :: M.Map (s, Action) Float
  , states  :: [s]
  , actions :: [Action]
  , epsilon :: Float
  , gen     :: StdGen
  } deriving Show

type AgentState = (Int, Int)

-- | Simply creates an agent from a game. Receives the epsilon of the
-- agent (probability of exploration)
agentFromGame :: FrozenLake -> Float -> Carlo AgentState
agentFromGame game eps=
  Carlo { policy  = M.fromList [(s, U) | s <- tiles]
        , qValues = M.fromList [((s, a), 0) | s <- tiles, a <- [U, D, L, R, KeepStill]]
        , states  = tiles
        , actions = [U, D, L, R, KeepStill]
        , epsilon = eps
        , gen     = mkStdGen 1}
  where tiles = getTileList game

policyToString :: Carlo AgentState -> (Int, Int) -> String
policyToString agent (n, m) =
  unlines $ map f [[(y, x) | x<-[0..n] ]| y<-[0..n]]
  where f l = join $ map g l
        g s = case policy agent M.!? s of
          Nothing         -> " ERRO "
          Just  D         -> "  D   "
          Just  R         -> "  ->  "
          Just  L         -> "  <-  "
          Just  U         -> "  U   "
          Just  KeepStill -> "  *   "

learn :: FrozenLake -> Carlo AgentState -> Int -> Int -> Float -> Carlo AgentState
learn g ag n l gamma =
  go g ag M.empty n l gamma
  where go _ agent _ 0 _  _ = agent
        go game agent rets n' l' gamm =
          let (newAgent, newReturns) = updateFromEpisode agent (reverse $ episode game agent l') gamm rets
          in go game newAgent newReturns (n'-1) l' gamm

-- | Input: An agent, An episode, a Float (gamma) denoting the learning rate
-- Output: The agent with the updated qValues
updateFromEpisode :: Carlo AgentState -> -- the agent
                     [(AgentState, Action, Float)] -> -- the episode
                     Float -> -- the learning rate
                     M.Map (AgentState, Action) [Float] -> -- the previous returns
                     (Carlo AgentState, M.Map (AgentState, Action) [Float])
updateFromEpisode ag ep gam rets =
  go ag ep rets 0 gam
  where go agent [] returns _ _ = (makeGreedy $ changeQValues agent returns, returns)
        go agent ((s, a, r):xs) returns ret gamma =
          if (s, a) `elem` [(s', a') | (s', a', _)<-xs]
          then go agent xs returns (gamma*ret + r) gamma
          else go agent xs (M.insertWith (++) (s, a) [gamma*ret + r] returns) (gamma*ret + r) gamma

changeQValues :: Carlo AgentState -> M.Map (AgentState, Action) [Float] -> Carlo AgentState
changeQValues agent returns =
  agent { qValues = newQValues }
  where newQValues = foldr (\x y -> M.updateWithKey f x y) (qValues agent) (M.keys returns)
        f key _ = do {l <- returns M.!? key ; return $  sum l / (fromIntegral $ length l) }

-- | Input: Initial game, Initial agent, number of moves
--   Output: An episode in the from state action reward
-- The episode is returned as [(s0, a0, r1),..., (sn-1, an-1, rn)].
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
                 then case policy agent M.!? s of
                        Nothing -> choice (actions agent) g
                        Just a  -> a
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
