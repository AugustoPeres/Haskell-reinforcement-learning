# Monte carlo reinforcement learning for the frozen lake

Because i did not feel like it i left the `main` function in the MonteCarlo.hs
file undefined. The code can just be run inside ghci. To see the agent learning
just use the following comands

```
λ> game  = makeGame (5, 5) 3 (mkStdGen 1)
λ> agent = agentFromGame game 0.3
λ> putStrLn $ gameToString game
  F    F    O    F    F
  F    F    F    F    F
  F    F    F    O    F
  G    F    F    F    F
  F    F    F    F    F

λ> newAgent = learn game agent 1000 1000 0.9
λ> putStrLn $ policyToString newAgent (4, 4)
  D     <-    <-    D     D
  D     D     D     <-    <-
  D     <-    D     D     D
  *     <-    <-    <-    <-
  U     U     U     U     <-
```

Here G denotes the goal, O denotes the holes and F denotes the normal tiles.

Regarding the actions \* denotes `KeepStill`. The others are self explanatory.


There are parts of the code that could use some efficiency improvements but
because this is just meant to run on dummy examples and writing good code is
harder we left it at that

For example we should not need to store an entire list of the returns for each
state action pair here

```
else go agent xs (M.insertWith (++) (s, a) [gamma*ret + r] returns) (gamma*ret + r) gamma
```

We should keep track of the means instead of storing this in memory.

