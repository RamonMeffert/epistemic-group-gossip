{-|
Module      : GossipTypes
Description : Implements different utilitarian types used for dynamic epistemic gossip, together with some operations thereon.
Copyright   : (c) Jesper Kuiper, 2021
                  Leander van Boven, 2021
                  Ramon Meffert, 2021
License     : BSD3
-}
module GossipTypes 
  ( -- * Type definitions
    -- ** Agents
    Agent
  , AgentId
  , AgentName
    -- ** Calls 
  , Call
  , GroupCall
    -- * Call generation
  , (☎)
  , toCalls
    -- * Showables and printables
    -- ** Agents
  , showAgent
    -- ** Calls
  , printCall
  , printCalls
  , printMakeCall
    -- * Group
  , printGroupCall
  , printGroupCalls
  , printMakeGroupCall
    -- ** General
  , printNoCalls
  , printAllCalls
  )

where

import Control.Monad
import Data.Graph.Inductive (Gr, LEdge, LNode, prettyPrint, Graph (noNodes))
import Data.List
import System.Console.ANSI

import Util

-- | The type of an agent. Agents are always defined in terms of a character and a corresponding (unique) identifying integer.
type Agent = LNode Char

-- | The agent identifier.
type AgentId = Int

-- | The agent name. 
type AgentName = Char

-- | A call between two agents, defined as (from/caller, to/callee).
-- A call is considered to be bidirectional, i.e. information flows from the caller to the callee and vice versa. 
type Call = (Agent, Agent)

-- | A call between a group of agents. The first item in the tuple is the caller, they need to have the phone number of all callees in the list. A group call is also omnidirectional, i.e. the all agent information is pooled and distributed to all agents in the call.  
type GroupCall = (Agent, [Agent])

-- | Generates a call between two agents. 
(☎) :: Agent -> Agent -> Call
(☎) f t = (f,t)
infix 0 ☎

-- | Converts a group call to a list of direct calls. This results in an identical flow of information. 
toCalls :: GroupCall -> [Call]
toCalls g@(f, to) = [(f,t) | t <- to] ++ [(t1,t2) | t1 <- to, t2 <- to, t1 /= t2]

-- | Converts an agent to a print-friendly string. 
showAgent :: Agent -> String
showAgent (_, lab) = [lab]

-- | Prints a message that no calls are available. 
printNoCalls :: IO ()
printNoCalls = do
  putStr "["
  putStrFgc Red "No calls to display"
  putStrLn "]"

-- | Prints a call. 
printCall :: Call -> IO ()
printCall ((_, i), (_, j)) = do
    putStr $ " (" ++ show i ++ "," ++ show j ++ ") "

-- | Prints a message that a call is made between two agents. 
printMakeCall :: Call -> IO ()
printMakeCall ((_,m), (_,n)) = do
    putStr "\nMaking call between "
    putStrFgc Green $ show m
    putStr " and "
    putStrLnFgc Green $ show n

-- | Prints a list of calls. 
printCalls :: [Call] -> IO ()
printCalls [] = printNoCalls
printCalls c = mapM_ printCall c

-- | Prints a group call. 
printGroupCall :: GroupCall -> IO ()
printGroupCall g = do
  putStrFgc Green "  Groupcall with calls: "
  mapM_ printCall (toCalls g)
  putStr "\n"

-- | Prints a message that a group call is made. 
printMakeGroupCall :: GroupCall -> IO ()
printMakeGroupCall ((_,f), to) = do
  putStr "\nMaking group call initialised by "
  putStrFgc Green [f]
  putStr " to: "
  putStrLnFgc Green $ intersperse ' ' $ map snd to

-- | Prints a list of group calls. 
printGroupCalls :: [GroupCall] -> IO ()
printGroupCalls [] = printNoCalls
printGroupCalls g = mapM_ printGroupCall g

-- | Prints all calls, given a list of direct calls and group calls. 
printAllCalls :: ([Call], [GroupCall]) -> IO ()
printAllCalls (c, g) = do
  putStrLnFgc Yellow "Direct calls:"
  printCalls c
  putStr "\n"
  putStrLnFgc Yellow "Group calls:"
  printGroupCalls g