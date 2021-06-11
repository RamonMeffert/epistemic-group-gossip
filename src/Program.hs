{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Program

( runProgram,
)

where

import System.Console.ANSI
import Data.Char ( isAlpha )
import Data.Graph.Inductive.Graph (hasLEdge)

import GossipTypes
import GossipGraph
import GossipState
import GossipProtocol
import GossipKnowledgeStructure
import Util
import ToDo

-- | Determines which color the (a) text is when requesting input from the user.  
-- Possible colors:  
-- - Black  
-- - Red  
-- - Green  
-- - Yellow  
-- - Blue  
-- - Magenta  
-- - Cyan  
-- - White
actionColor :: Color
actionColor = Blue

-- | Main entry point to the program.
-- Parses the gossipgraph and allows user to select what 
runProgram :: IO ()
runProgram = do
  -- parse GossipGraph from input (cli or txt)
  -- during testing, uncomment line below:
  tg <- obtainTestGraph
  let initState = State tg (fromGossipGraph tg) []
  putStrLn "State initialized..."
  runAction initState
  where
    obtainTestGraph :: IO GossipGraph
    obtainTestGraph = do
      putStrLn "testGraph(1) or testGraph(2)?"
      g <- getLine
      case g of
        "1" -> return testGraph
        "2" -> return testGraph2
        other -> do
          printInvalidAction other
          obtainTestGraph

    printProtocols :: IO ()
    printProtocols = do
      putStrLn "\nWhat protocol would you like to use?"
      putStr "Call-("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "a"
      setSGR [Reset]
      putStr ")ny or ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "l"
      setSGR [Reset]
      putStr ")earn-new-secrets?"

    obtainProtocol :: IO GossipProtocol
    obtainProtocol = do
      printProtocols
      prot <- getLine
      case prot of
        "a" -> return callAny
        "l" -> return learnNewSecrets
        other -> do
          printInvalidAction other
          obtainProtocol

    printOperationModes :: IO ()
    printOperationModes = do
      putStrLn "\nWhat program operation mode must be used?"
      putStr "Use ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "u"
      setSGR [Reset]
      putStr ")seractions, ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "p"
      setSGR [Reset]
      putStr ")rotocol or ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "h"
      setSGR [Reset]
      putStrLn ")ybrid?"

    runAction :: State -> IO ()
    runAction s = do
      printOperationModes
      a <- getLine
      case a of
        "u" -> userActions s
        "p" -> do
          p <- obtainProtocol
          protocolActions p s
        "h" -> do
          p <- obtainProtocol
          hybridActions p s
        other -> do
          printInvalidAction other
          runAction s

-- | Combines both user and protocol actions into one function.
-- Each tick the user may choose to perform a custom action, protocol action or first custom and then protocol action.
hybridActions :: GossipProtocol -> State -> IO ()
hybridActions prot state = do
  newState <- executeAction
  hybridActions prot newState
  where
    printHybridActions :: IO ()
    printHybridActions = do
      putStrLn "Which type of actions would you like to perform?"
      putStr "Only ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "u"
      setSGR [Reset]
      putStr ")ser actions, only ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "p"
      setSGR [Reset]
      putStr ")rotocol actions or ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "b"
      setSGR [Reset]
      putStr ")oth?"

    executeAction :: IO State
    executeAction = do
      printHybridActions
      a <- getLine
      case a of
        "u" -> performUserAction state
        "p" -> performProtocolAction prot state
        "b" -> do
          s <- performUserAction state
          performProtocolAction prot s
        other -> do
          printInvalidAction other
          executeAction

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
    printCallNotAllowed :: Agent -> Agent -> IO ()
    printCallNotAllowed (_,f) (_,t) = putStrLn $ "Call between " ++ show f ++ " and " ++ show t ++ " is not allowed as -N(" ++ show f ++ "," ++ show t ++ ")!"

    printInvalidCalls :: State -> [Call] -> IO ()
    printInvalidCalls state calls = do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Invalid calls:"
      setSGR [Reset]
      mapM_ (uncurry printCallNotAllowed) [c | c@((i, _), (j, _)) <- calls, not (hasLEdge (stateGraph state) (i, j, Number))]

    createCall :: State -> Char -> Char -> Call
    createCall state f t =  (flip agent f $ labToId f , flip agent t $ labToId t)

    callsAllowed :: State -> [Call] -> Bool
    callsAllowed state calls = all (\ ((i,_), (j, _)) -> hasLEdge (stateGraph state) (i, j, Number)) calls

    obtainCallDetails :: State -> IO [Call]
    obtainCallDetails s = do
      putStrLn "\nWho is calling?"
      fromStr <- getLine
      putStrLn "Who is being called? (multiple agents for groupcall)"
      toStr <- getLine
      let to = filter isAlpha toStr
      let mainCalls = map (createCall s $ head fromStr) to
      if callsAllowed s mainCalls
        then return $ map (createCall s $ head fromStr) to ++ [createCall s ta tb | ta <- to, tb <- to, ta /= tb]
        else do
          printInvalidCalls s mainCalls
          putStrLn "Resubmit call details..."
          obtainCallDetails s

    executeCalls :: State -> [Call] -> IO State
    executeCalls s [] = return s
    executeCalls s (c:cs) = do
        printMakeCall c
        let newState = makeCall c s
        printState newState True
        executeCalls newState cs

    printUserActions :: IO ()
    printUserActions = do
      putStrLn "\nWhat action would you like to perform?"
      putStr "Make a ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "c"
      setSGR [Reset]
      putStr ")all, view ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "p"
      setSGR [Reset]
      putStr ")ossible calls or view current ("
      setSGR [SetColor Foreground Vivid actionColor]
      putStr "s"
      setSGR [Reset]
      putStrLn ")tate?"

    executeUserAction :: State -> IO State
    executeUserAction s = do
      printUserActions
      a <- getLine
      case a of 
        "c" -> do
          c <- obtainCallDetails s
          executeCalls s c
        "p" -> do
          printCalls $ validCalls $ stateGraph s
          putStr "\n"
          return s
        "s" -> do
          printState s False
          return s
        other -> do
          printInvalidAction other
          executeUserAction s

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
  printState state True
  return state
      