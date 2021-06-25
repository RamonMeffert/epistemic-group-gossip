{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Program

( runProgram,
)

where

import Control.Monad (when)
import System.Console.ANSI
import Data.Char ( isAlpha, toLower )
import Data.Graph.Inductive.Graph (hasLEdge, edges)

import GossipTypes
import GossipGraph
import GossipState
import GossipProtocol
import GossipKnowledge
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
      putStrLn "testGraph(1), testGraph(2) or biggerGraph(3)?"
      g <- getLine
      case toLower $ head g of
        '1' -> return testGraph
        '2' -> return testGraph2
        '3' -> return biggerGraph
        other -> do
          printInvalidAction other
          obtainTestGraph

    printProtocols :: IO ()
    printProtocols = do
      putStrLn "\nWhat protocol would you like to use?"
      putStr "Call-("
      putStrFgc actionColor "a"
      putStr ")ny, ("
      putStrFgc actionColor "l"
      putStrLn ")earn-new-secrets or"
      putStr "("
      putStrFgc actionColor "p"
      putStrLn ")ossible-information-growth?"

    obtainProtocol :: IO GossipProtocol
    obtainProtocol = do
      printProtocols
      prot <- getLine
      case toLower $ head prot of
        'a' -> return callAny
        'l' -> return learnNewSecrets
        'p' -> return possibleInformationGrowth
        other -> do
          printInvalidAction other
          obtainProtocol

    printOperationModes :: IO ()
    printOperationModes = do
      putStrLn "\nWhat program operation mode must be used?"
      putStr "Use ("
      putStrFgc actionColor "u"
      putStr ")seractions, ("
      putStrFgc actionColor "p"
      putStr ")rotocol or ("
      putStrFgc actionColor "h"
      putStrLn ")ybrid?"

    runAction :: State -> IO ()
    runAction s = do
      printOperationModes
      a <- getLine
      case toLower $ head a of
        'u' -> userActions s
        'p' -> do
          p <- obtainProtocol
          protocolActions p s
        'h' -> do
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
      putStrFgc actionColor "u"
      putStr ")ser actions, only ("
      putStrFgc actionColor "p"
      putStr ")rotocol actions or ("
      putStrFgc actionColor "b"
      putStrLn ")oth?"

    executeAction :: IO State
    executeAction = do
      printHybridActions
      a <- getLine
      case toLower $ head a of
        'u' -> performUserAction state
        'p' -> performProtocolAction prot state
        'b' -> do
          s <- performUserAction state
          performProtocolAction prot s
        other -> do
          printInvalidAction other
          executeAction

-- | Continuous execution of user-action.
userActions :: State -> IO ()
userActions state = do
  newState <- performUserAction state
  if isGraphComplete (stateGraph newState)
    then do
      continue <- requestContinuation
      when continue $ userActions newState
    else userActions newState  -- Recursive call
  where
    requestContinuation :: IO Bool
    requestContinuation = do
      printGraphComplete
      putStr "Would you like to continue? ("
      putStrFgc actionColor "y"
      putStr " / "
      putStrFgc actionColor "n"
      putStrLn ")"
      a <- getLine
      return $ toLower (head a) == 'y'

-- | Execute action against GossipGraph:
  -- Update GossipGraph
  -- -> Present user with new state (i.e. valuation of observables, current knowledge)
  -- -> Display implications of the performed action (i.e. what would be rational actions by the agents).
performUserAction :: State -> IO State
performUserAction = executeUserAction
  where
    printCallNotAllowed :: Agent -> Agent -> IO ()
    printCallNotAllowed (_,f) (_,t) = do
      putStr "Call between "
      putStrFgc Red $ show f
      putStr " and "
      putStrFgc Red $ show t
      putStrLn $ " is not allowed as N" ++ [f] ++ [t] ++ " is not in the statelaw."

    printInvalidCalls :: State -> [Call] -> IO ()
    printInvalidCalls state calls = do
      putStrLnFgc Red "Invalid calls:"
      mapM_ (uncurry printCallNotAllowed) [c | c@((i, _), (j, _)) <- calls, not (hasLEdge (stateGraph state) (i, j, Number))]

    createCall :: Char -> Char -> Call
    createCall f t = agentFromLab f ☎ agentFromLab t

    callsAllowed :: State -> [Call] -> Bool
    callsAllowed state calls = all (\ ((i,_), (j, _)) -> hasLEdge (stateGraph state) (i, j, Number)) calls

    obtainCallDetails :: State -> IO [Call]
    obtainCallDetails s = do
      putStrLn "\nWho is calling?"
      fromStr <- getLine
      putStrLn "Who is being called? (multiple agents for groupcall)"
      toStr <- getLine
      let to = filter isAlpha toStr
      let mainCalls = map (createCall $ head fromStr) to
      if callsAllowed s mainCalls
        then return $ mainCalls ++ [createCall ta tb | ta <- to, tb <- to, ta /= tb]
        else do
          printInvalidCalls s mainCalls
          putStrLn "Resubmit call details..."
          obtainCallDetails s

    printUserActions :: IO ()
    printUserActions = do
      putStrLn "\nWhat action would you like to perform?"
      putStr "Make a ("
      putStrFgc actionColor "c"
      putStr ")all, view ("
      putStrFgc actionColor "p"
      putStr ")ossible calls or view current ("
      putStrFgc actionColor "s"
      putStrLn ")tate?"

    executeUserAction :: State -> IO State
    executeUserAction s = do
      printUserActions
      a <- getLine
      case toLower $ head a of
        'c' -> do
          c <- obtainCallDetails s
          executeCalls c s
        'p' -> do
          printAllCalls $ validCalls $ stateGraph s
          putStr "\n"
          return s
        's' -> do
          printState s False
          return s
        other -> do
          printInvalidAction other
          executeUserAction s

-- | Continuous execution of protocol-actions.
protocolActions :: GossipProtocol -> State -> IO ()
protocolActions prot state = do
  newState <- performProtocolAction prot state
  if isGraphComplete (stateGraph newState)
    then do
      printGraphComplete
      putStrLnFgc Red "Stopping protocol..."
      continue <- requestContinuation
      when continue $ userActions newState
    else protocolActions prot newState  -- Recursive call

  where
    requestContinuation :: IO Bool
    requestContinuation = do
      putStr "Would you like to continue with useractions? ("
      putStrFgc actionColor "y"
      putStr " / "
      putStrFgc actionColor "n"
      putStrLn ")"
      a <- getLine
      return $ toLower (head a) == 'y'

-- | Perform protocol tick:
  -- Explain the to be performed actions
  -- -> performProtocolTick
  -- -> Present user with new state (i.e. valuation of observables, current knowledge).
performProtocolAction :: GossipProtocol -> State -> IO State
performProtocolAction prot state = do
  let calls = selectedCalls prot state

  putStrLnFgc Yellow "\nCalls allowed by the protocol for this tick:"
  printAllCalls calls

  newState <- executeCall calls state
  printState newState True

  putStrLnFgc Green "\nPress enter for next tick."
  _ <- getLine

  return newState

  where
    executeDirectCall :: [Call] -> State -> IO State
    executeDirectCall d s = do
      printMakeCall $ head d
      putStrLn ""
      return $ flip makeCall s $ head d

    executeGroupCall :: [GroupCall] -> State -> IO State
    executeGroupCall g s = do
      putStrLnFgc Green "Making group call:"
      printMakeGroupCall $ head g
      flip executeCalls s $ toCalls $ head g

    getCallType :: IO String
    getCallType = do
      putStrLn "Both direct and group calls possible."
      putStr "Perform ("
      putStrFgc actionColor "d"
      putStr ")irect call or ("
      putStrFgc actionColor "g"
      putStrLn ")roup call?"
      getLine

    executeCall :: ([Call], [GroupCall]) -> State -> IO State
    -- No calls possible:
    executeCall ([], []) s = do
      putStrLn "As there is no call allowed, the state will not be updated..."
      return s
    -- Only direct calls possible:
    executeCall (d, []) s = executeDirectCall d s
    -- Only groupcalls possible:
    executeCall ([], g) s = executeGroupCall g s
    -- Both direct and groupcalls possible:
    executeCall (d,g) s = do
      t <- getCallType
      case toLower $ head t of
        'd' -> executeDirectCall d s
        'g' -> executeGroupCall g s