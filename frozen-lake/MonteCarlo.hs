import           Control.Monad.State
import qualified Data.Map.Lazy       as M
import           Data.Maybe
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
  makeGreedy $
  Carlo { policy  = M.fromList [(s, U) | s <- tiles]
        , qValues = M.fromList [((s, a), 0) | s <- tiles, a <- [U, D, L, R, KeepStill]]
        , states  = tiles
        , actions = [U, D, L, R, KeepStill]
        , epsilon = eps
        , gen     = mkStdGen 1}
  where tiles = getTileList game

-- Prints a policy in a way that is readable for the particular case of the
-- frozen lake environment.
policyToString :: Carlo AgentState -> (Int, Int) -> String
policyToString agent (n, m) =
  unlines $ map f [[(x, y) | x<-[0..n] ]| y<-[0..m]]
  where f l = join $ map g l
        g s = case policy agent M.!? s of
          Nothing         -> " ERRO "
          Just  D         -> "  D   "
          Just  R         -> "  ->  "
          Just  L         -> "  <-  "
          Just  U         -> "  U   "
          Just  KeepStill -> "  *   "

-- | Receives:
--    * A frozen lake
--    * An agent
--    * The number of episodes
--    * The length of each episode
--    * The learning rate
--  Returns: An agent after having learned the proper policy
learn :: FrozenLake -> Carlo AgentState -> Int -> Int -> Float -> Carlo AgentState
learn g ag n l gamma =
  go g ag M.empty n l gamma
  where go _ agent _ 0 _  _ = agent
        go game agent rets n' l' gamm =
          let --(newAgent, newReturns) = updateFromEpisode agent (reverse $ episode game agent l') gamm rets
              (ep, (newGame, newAgent)) = runState (episode l) (game, agent) -- this way we also update the gens
              (newAgent', newReturns) = updateFromEpisode newAgent (reverse ep) gamm rets
          in go newGame newAgent' newReturns (n'-1) l' gamm

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

-- This is just an aux function that receives the returns and an agent and
-- changes the Q values of each action state pair
changeQValues :: Carlo AgentState -> M.Map (AgentState, Action) [Float] -> Carlo AgentState
changeQValues agent returns =
  agent { qValues = newQValues }
  where newQValues = foldr (\x y -> M.updateWithKey f x y) (qValues agent) (M.keys returns)
        f key _ = do {l <- returns M.!? key ; return $  sum l / (fromIntegral $ length l) }

-- | Uses episode and extracts the values from the state monad to generate an
-- episode
evalEpisode :: FrozenLake -> Carlo AgentState -> Int -> [(AgentState, Action, Float)]
evalEpisode game agent n = evalState (episode n) (game, agent)

-- | Input: Initial game, Initial agent, number of moves
--   Output: An episode in the from state action reward
-- The episode is returned as [(s0, a0, r1),..., (sn-1, an-1, rn)].
episode :: Int -> State (FrozenLake, Carlo AgentState) [(AgentState, Action, Float)]
episode n = forM [1..n] (\_ -> agentTurn)

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
  agent { policy = M.fromList $ evalState (mapM getMaximumAction (states agent)) agent }
--  agent { policy = M.fromList [(s, getMaximumAction agent s) | s <- states agent]}

-- | For a given state returns the agent with largest Q value
-- getMaximumAction :: (Ord s) => Carlo s -> s -> Action
-- getMaximumAction agent st =
--   maximumBy (comparing f) (actions agent)
--   where f a = fromMaybe 0 $ qValues agent M.!? (st, a)

getMaximumAction :: (Ord s) => s -> State (Carlo s) (s, Action)
getMaximumAction st = do
  agent <- get
  let g = gen agent
      ma = maxValuesBy (actions agent) (\x -> fromMaybe 0 $ qValues agent M.!? (st, x))
      (i, ng) = sampleState (uniform 0 (-1 + length ma)) g
      a = ma !! i
  put $ agent {gen = ng}
  return (st, a)

maxValuesBy :: (Ord b) => [a] -> (a -> b) -> [a]
maxValuesBy [] f = undefined
maxValuesBy (x:xs) f =
  go [x] xs
  where go l [] = l
        go l@(y:xy) (a:xa)
          | f y < f a  = go [a] xs
          | f y == f a = go (a:l) xa
          | otherwise  = go l xa

-- | Chooses a random element from a list
choice :: [a] -> StdGen -> a
choice l g = l!!(fst $ sampleState (uniform 0 (-1 + length l)) g)

main :: IO()
main = undefined
