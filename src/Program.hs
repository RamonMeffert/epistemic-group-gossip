{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Program

( runProgram,
)

where

import Data.Char ( isAlpha )
import Data.Graph.Inductive.Graph (hasLEdge)
import GossipGraph
import GossipProtocol
import GossipKnowledgeStructure

-- | Main entry point to the program.
-- Parses the gossipgraph and allows user to select what 
runProgram :: IO ()
runProgram = do
  -- parse GossipGraph from input (cli or txt)
  -- during testing, uncomment line below:
  tg <- obtainTestGraph
  let initState = State tg (fromGossipGraph tg) []
  putStrLn "State initialized..."
  putStrLn "Useractions (u), Protocol (p) or Hybrid (h)?"
  action <- getLine
  runActions initState action
  where
    obtainTestGraph :: IO GossipGraph
    obtainTestGraph = do
      putStrLn "testGraph(1) or testGraph(2)?"
      g <- getLine
      return $ case g of
        "1" -> testGraph
        "2" -> testGraph2

    obtainProtocol :: IO GossipProtocol
    obtainProtocol = do
      putStrLn "What protocol would like to use?"
      putStrLn "(a)ny, (l)earn new secrets"
      prot <- getLine
      return $ case prot of
        "a" -> (\ _ _ -> True)
        "l" -> (\ _ _ -> False)

    runActions s a | a == "u" = userActions s
                   | a == "p" = do
                      p <- obtainProtocol
                      protocolActions p s
                   | a == "h" = do
                      p <- obtainProtocol
                      hybridActions p s

-- | Combines both user and protocol actions into one function.
-- Each tick the user may choose to perform a custom action, protocol action or first custom and then protocol action.
hybridActions :: GossipProtocol -> State -> IO ()
hybridActions prot state = do
  putStrLn "Actiontype? (u)ser, (p)rotocol or (b)oth"
  action <- getLine
  newState <- executeAction action
  hybridActions prot newState
  where
    executeAction a | a == "u" = performUserAction state
                    | a == "p" = performProtocolAction prot state
                    | a == "b" = do
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
performUserAction = executeUserAction
  where
    obtainUserAction :: IO String
    obtainUserAction = do
      putStrLn ""
      putStrLn "What action would you like to perform?"
      putStrLn "Make a (c)all, view (p)ossible calls or view current (s)tate?"
      getLine

    printCallNotAllowed :: Agent -> Agent -> IO ()
    printCallNotAllowed f t = putStrLn $ "Call between " ++ show f ++ " and " ++ show t ++ " is not allowed as -N(" ++ show f ++ "," ++ show t ++ ")!"

    createCall :: State -> Char -> Char -> Call
    createCall state f t =(flip agent f $ labToId f , flip agent t $ labToId t)

    obtainCallDetails :: State -> IO [Call]
    obtainCallDetails s = do
      putStrLn "Who is calling?"
      fromStr <- getLine
      putStrLn "Who's is being called? (multiple agents for groupcall)"
      toStr <- getLine
      let to = filter isAlpha toStr
      return $ map ( createCall s $ head fromStr) to ++ [createCall s ta tb | ta <- to, tb <- to, ta /= tb]

    executeCalls :: State -> [Call] -> IO State
    executeCalls s [] = return s
    executeCalls s (c@(f@(i, _),t@(j, _)):cs) = do
        putStrLn $ "Making call between " ++ show f ++ " and " ++ show t
        if not (hasLEdge (stateGraph s) (i, j, Number))
          then do
            printCallNotAllowed f t
            executeCalls s cs
          else do
            let newState = makeCall c s
            putStrLn "=== Updated state ==="
            printState newState
            executeCalls newState cs

    obtainNewState :: State -> String -> IO State
    obtainNewState s a | a == "c" = do
                          -- TODO: print possible calls?
                         calls <- obtainCallDetails s
                         putStrLn $ "calls: " ++ show calls
                         executeCalls s calls
                       | a == "p" = do
                         printCalls $ validCalls $ stateGraph s
                         return s
                       | a == "s" = do
                         printState s
                         return s

    executeUserAction :: State -> IO State
    executeUserAction s = do
      action <- obtainUserAction
      obtainNewState s action

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
  putStrLn "=== Updated state ==="
  printState state
  return state
      