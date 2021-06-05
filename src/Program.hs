module Program 

( runProgram,
)

where

import GossipGraph
import GossipProtocol

runProgram :: IO ()
runProgram = do
  -- Parse Gossip graph
  -- during testing, uncomment line below:
  let graph = testGraph 
  putStrLn "Gossip graph parsed"
  putStrLn "Useractions (u) or Protocol (p)?"
  action <- getLine 
  runActions graph action 
  where 
    runActions graph action | action == "u" = userActions graph
                            | action == "p" = protocolActions 1 graph

userActions :: GossipGraph -> IO ()
userActions graph = do
  putStrLn "What action would you like to perform?"
  action <- getLine
  putStrLn ("You want to " ++ action ++ "!?")
  let newGraph = performUserAction graph action
  _ <- printGraph newGraph
  userActions newGraph
  where
    performUserAction :: GossipGraph -> String -> GossipGraph
    performUserAction g a = do
      -- Execute action against GossipGraph
        -- Update GossipGraph
        -- Present user with new state (i.e. valuation of observables, current knowledge)
        -- Display implications of the performed action (i.e. what would be rational actions by the agents)    
      g

protocolActions :: GossipProtocol -> GossipGraph -> IO ()
protocolActions prot graph = do
  -- Perform protocol tick
    -- Explain the to be performed actions
    -- Update GossipGraph
    -- Present user with new state (i.e. valuation of observables, current knowledge)
  protocolActions prot $ performProtocolTick prot graph