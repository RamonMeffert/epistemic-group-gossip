module Program 

( runProgram,
)

where

import GossipGraph
import GossipProtocol
import GossipKnowledgeStructure

-- | Main entry point to the program.
-- Parses the gossipgraph and allows user to select what 
runProgram :: IO ()
runProgram = do
  -- parse GossipGraph from input (cli or txt)
  -- during testing, uncomment line below:
  let initState = State testGraph (fromGossipGraph testGraph) []
  putStrLn "State parsed"
  putStrLn "Useractions (u), Protocol (p) or Hybrid (h)?"
  action <- getLine 
  runActions initState action 
  where 
    obtainProtocol :: IO GossipProtocol
    obtainProtocol = do
      putStrLn "What protocol would like to use?"
      putStrLn "(a)ny, (l)earn new secrets"
      prot <- getLine
      return $ case prot of
        "a" -> (\ _ _ -> True)
        "l" -> (\ _ _ -> False)
    runActions state action =
      case action of 
        "u" -> userActions state
        "p" -> do
          prot <- obtainProtocol
          protocolActions prot state
        "h" -> do
          prot <- obtainProtocol
          hybridActions prot state

-- | Combines both user and protocol actions into one function.
-- Each tick the user may choose to perform a custom action, protocol action or first custom and then protocol action.
hybridActions :: GossipProtocol -> State -> IO ()
hybridActions prot state = do
  putStrLn "Actiontype? (u)ser, (p)rotocol or (b)oth"
  action <- getLine 
  newState <- executeAction action
  hybridActions prot newState
  where 
    executeAction a =
      case a of 
        "u" -> performUserAction state
        "p" -> performProtocolAction prot state
        "b" -> do
          newState <- performUserAction state
          performProtocolAction prot newState
  
-- | Continuous execution of user-action.
userActions :: State -> IO ()
userActions state = do 
  newState <- performUserAction state
  userActions newState  -- Recursive call

-- | Execute action against GossipGraph:
  -- Update GossipGraph
  -- -> Present user with new state (i.e. valuation of observables, current knowledge)
  -- -> Display implications of the performed action (i.e. what would be rational actions by the agents).
performUserAction :: State -> IO State 
performUserAction state =
  executeUserAction state obtainUserAction
  where
    obtainUserAction :: IO String
    obtainUserAction = do
      putStrLn "What action would you like to perform?"
      action <- getLine
      putStrLn ("You want to " ++ action ++ "!?")
      return action

    executeUserAction :: State -> IO String -> IO State
    executeUserAction g a = do
      action <- a
      let newState = makeCall (agent 1 'a', agent 2 'b') g  -- Do something with action
      putStrLn "Updated state:"
      printGraph $ stateGraph newState
      return newState

-- | Continuous execution of protocol-actions.
protocolActions :: GossipProtocol -> State -> IO ()
protocolActions prot state = do
  newState <- performProtocolAction prot state
  protocolActions prot newState  -- Recursive call

-- | Perform protocol tick:
  -- Explain the to be performed actions
  -- -> performProtocolTick
  -- -> Present user with new state (i.e. valuation of observables, current knowledge).
performProtocolAction :: GossipProtocol -> State -> IO State 
performProtocolAction prot state = do
  let state = performProtocolTick prot state
  putStrLn "Updated state:"
  printGraph $ stateGraph state
  return state
      