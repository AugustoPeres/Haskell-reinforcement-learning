import System.Random
import Data.Random
import Control.Monad.State


-- | Use the state monad so that we can compose several run of the arm
data Arm = Arm { mean      :: Float
               , deviation :: Float
               , gen       :: StdGen}
         deriving Show

-- | pullArm :: State Arm Float is pretty much equivalent to a function
--   arm -> (reward, arm). Therefore we simply define pull arm as being a function
--   that receives an arm and then returns a tuple containing the reward together
--   with another arm with the gen updated
pullArm :: State Arm Float
pullArm =
  state (\s -> let (val, g) = sampleState (normal (mean s) (deviation s)) (gen s)
               in (val, s {gen = g}))

getRewardSequence :: Arm -> Int -> [Float]
getRewardSequence a n = evalState (replicateM n pullArm) a

main :: IO ()
main = undefined
