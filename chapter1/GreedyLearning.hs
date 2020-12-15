import           Control.Monad.State
import           Data.Random
import           System.Random
import Data.List (maximumBy)
import Data.Ord (comparing)


type Action = Int
type Reward = Float

-- | Use the state monad so that we can compose several run of the arm
data Arm = Arm { mean      :: Float
               , deviation :: Float
               , gen       :: StdGen}
         deriving Show

data Bandit = Bandit { arms    :: [Arm]  -- a list with several arms
                     , actions :: [Action]} -- a list with the possible arms that can be pulled
              deriving Show

data GreedyLearner = GreedyLearner { qFunction :: [(Action, Reward)] -- list with action reward
                                   , epsilon   :: Float
                                   , alpha     :: Float
                                   , acts      :: [Action] -- actions available to the agent
                                   , currGen   :: StdGen}
                          deriving Show

-- | Receives an action and a rewards
--   Updates the qFunction of the agent
updateQFunction :: GreedyLearner -> Action -> Reward -> State GreedyLearner ()
updateQFunction learner a r =
  put newLearner
  where newQFunction = [if action /= a
                        then (action, reward)
                        else (action, reward + (alpha learner) * (r - reward))
                       | (action, reward) <- qFunction learner]
        newLearner = learner { qFunction = newQFunction }

-- | Gives a learner as a state monad
getAction :: State GreedyLearner Action
getAction =
  state (\s -> let (val, newGen) = sampleState (uniform 0 1) (currGen s)
                   action = if val >= epsilon s
                            then greedy (qFunction s)
                            else choice (qFunction s) (currGen s)
               in (action, s {currGen = newGen}))

-- | Returns the index int corresponding to the action with greater value
greedy :: [(Action, Reward)] -> Action
greedy l = fst $ maximumBy (comparing snd) l

-- | Receives a list and an StdGen and returns a random action
choice :: [(Action, Reward)] -> StdGen -> Action
choice l g =
  fst $ l!!i
  where i = fst $ sampleState (uniform 0 (length l - 1)) g

-- | Input: An arm to pull
--   Output: The resulting bandit and the obtained reward as state
playBandit :: Action -> State Bandit Reward
playBandit n =
  state (\s -> let arm = arms s!!n
                   (val, newArm) = runState pullArm arm
                   newArmList = [if i == n then newArm else arms s!!i | i <- [0..(length(arms s)-1)]]
               in (val, s {arms = newArmList}))

-- | Input: A bandit and a sequence of integers(arms to be pulled)
--   Output: A list with rewards
--   This serves just to understand the state monad. Is not used in the learning
getRewardsBandit :: Bandit -> [Action] -> [Reward]
getRewardsBandit b actions =
  -- mapM :: (action -> (State Bandit) Reward) -> [actions] -> (State Bandit) [Reward]
  -- this is equivalent to: evalState (sequence (map playBandit) action) b =
  --                        evalState (sequence [playBandit a1, ..., playBandit an]) b =
  --                        evalState (sequence [(State Bandit) Reward, ..., (State Bandit) Reward]) b =
  --                        evalState ((State Bandit) [Reward]) b
  -- sequence :: Monad m => [m a] -> m [a]
  -- sequence [] = return [] -- state (\s -> ([], s))
  -- sequence (x:xs) = do
  --                 z <- x
  --                 y <- sequence xs
  --                 return (z:y) -- state (\s -> (z:y, s))
  evalState (mapM (playBandit) actions) b

-- | pullArm :: State Arm Reward is pretty much equivalent to a function
--   arm -> (reward, arm). Therefore we simply define pull arm as being a function
--   that receives an arm and then returns a tuple containing the reward together
--   with another arm with the gen updated
pullArm :: State Arm Reward
pullArm =
  state (\s -> let (val, g) = sampleState (normal (mean s) (deviation s)) (gen s)
               in (val, s {gen = g}))

-- | Plays an arm n times and returns the rewards. This is not used
--   in the learning. It just serves to help me understand the state monad.
getRewardSequence :: Arm -> Int -> [Reward]
getRewardSequence a n = evalState (replicateM n pullArm) a

-- | Input: The arm, the learner, the number of plays
--   Output: A list with [(action, reward)]
learn :: Bandit -> GreedyLearner -> Int -> [(Action, Reward)]
learn b agent n =
  evalState (replicateM n onePass) (agent, b)

-- | Makes one pass of the game
onePass :: State (GreedyLearner, Bandit) (Action, Reward)
onePass = do
  (learner, bandit) <- get
  let (action, newAgent) = runState getAction learner
      reward = evalState (playBandit action) bandit
      newBandit = execState (playBandit action) bandit
      newAgent' = execState  (updateQFunction newAgent action reward) learner
  put $ (newAgent', newBandit)
  return (action, reward)

arm0 = Arm 1 1 (mkStdGen 1)
arm2 = Arm 2 1 (mkStdGen 2)
arm3 = Arm 0 0.1 (mkStdGen 3)
bandit = Bandit [arm0, arm2, arm3] [0..2]
learner = GreedyLearner [(a, 0) | a <- [0..2]] 0.2 0.1 [0..2] (mkStdGen 4)

main :: IO ()
main = putStrLn $ show (learn bandit learner 500)
