module Program 

( runProgram,
)

where

import GossipGraph
import GossipProtocol

-- | Main entry point to the program.
-- Parses the gossipgraph and allows user to select what 
runProgram :: IO ()
runProgram = do
  -- Parse Gossip graph
  -- during testing, uncomment line below:
  let graph = testGraph 
  putStrLn "Gossip graph parsed"
  putStrLn "Useractions (u), Protocol (p) or Hybrid (h)?"
  action <- getLine 
  runActions graph action 
  where 
    runActions graph action | action == "u" = userActions graph
                            | action == "p" = protocolActions 1 graph
                            | action == "h" = hybridActions 1 graph

-- | Combines both user and protocol actions into one function.
-- Each tick the user may choose to perform a custom action, protocol action or first custom and then protocol action.
hybridActions :: GossipProtocol -> GossipGraph -> IO ()
hybridActions prot graph = do
  putStrLn "Actiontype? (u)ser, (p)rotocol or (b)oth"
  action <- getLine 
  newGraph <- executeAction action
  hybridActions prot newGraph
  where 
    executeAction a=
      case a of 
        "u" -> performUserAction graph
        "p" -> performProtocolAction prot graph
        "b" -> do
          newGraph <- performUserAction graph
          performProtocolAction prot newGraph
  
-- | Continuous execution of user-action.
userActions :: GossipGraph -> IO ()
userActions graph = do 
  newGraph <- performUserAction graph
  userActions newGraph  -- Recursive call

-- | Execute action against GossipGraph:
  -- Update GossipGraph
  -- -> Present user with new state (i.e. valuation of observables, current knowledge)
  -- -> Display implications of the performed action (i.e. what would be rational actions by the agents).
performUserAction :: GossipGraph -> IO GossipGraph 
performUserAction graph =
  executeUserAction graph obtainUserAction
  where
    obtainUserAction :: IO String
    obtainUserAction = do
      putStrLn "What action would you like to perform?"
      action <- getLine
      putStrLn ("You want to " ++ action ++ "!?")
      return action

    executeUserAction :: GossipGraph -> IO String -> IO GossipGraph
    executeUserAction g a = do
      action <- a
      let newGraph = g  -- Do something with action
      return newGraph

-- | Continuous execution of protocol-actions.
protocolActions :: GossipProtocol -> GossipGraph -> IO ()
protocolActions prot graph = do
  newGraph <- performProtocolAction prot graph
  protocolActions prot newGraph  -- Recursive call

-- | Perform protocol tick:
  -- Explain the to be performed actions
  -- -> performProtocolTick
  -- -> Present user with new state (i.e. valuation of observables, current knowledge).
performProtocolAction :: GossipProtocol -> GossipGraph -> IO GossipGraph 
performProtocolAction prot graph = do
  let graph = performProtocolTick prot graph
  putStrLn "Updated graph:"
  printGraph graph
  return graph
      