module GossipTypes where

import Data.List
import Control.Monad
import System.Console.ANSI
import Data.Graph.Inductive (Gr, LEdge, LNode, prettyPrint, Graph (noNodes))

import Util

type Agent = LNode Char
type AgentId = Int
type AgentName = Char

-- | (from, to).
-- Call is considered to be bidirectional,
-- Optionally we could add a direction component to allow singe directional broadcasting calls.
type Call = (Agent, Agent)

type GroupCall = (Agent, [Agent])

(☎) :: Agent -> Agent -> Call
(☎) f t = (f,t)
infix 0 ☎

toCalls :: GroupCall -> [Call]
toCalls g@(f, to) = [(f,t) | t <- to] ++ [(t1,t2) | t1 <- to, t2 <- to, t1 /= t2]

showAgent :: Agent -> String
showAgent (_, lab) = [lab]

printNoCalls :: IO ()
printNoCalls = do
  putStr "["
  putStrFgc Red "No calls to display"
  putStrLn "]"

printCall :: Call -> IO ()
printCall ((_, i), (_, j)) = do
    putStr $ " (" ++ show i ++ "," ++ show j ++ ") "

printMakeCall :: Call -> IO ()
printMakeCall ((_,m), (_,n)) = do
    putStr "\nMaking call between "
    putStrFgc Green $ show m
    putStr " and "
    putStrLnFgc Green $ show n

printCalls :: [Call] -> IO ()
printCalls [] = printNoCalls
printCalls c = mapM_ printCall c

printGroupCall :: GroupCall -> IO ()
printGroupCall g = do
  putStrFgc Green "  Groupcall with calls: "
  mapM_ printCall (toCalls g)
  putStr "\n"

printMakeGroupCall :: GroupCall -> IO ()
printMakeGroupCall ((_,f), to) = do
  putStr "\nMaking group call initialised by "
  putStrFgc Green [f]
  putStr " to: "
  putStrLnFgc Green $ intersperse ' ' $ map snd to

printGroupCalls :: [GroupCall] -> IO ()
printGroupCalls [] = printNoCalls
printGroupCalls g = mapM_ printGroupCall g

printAllCalls :: ([Call], [GroupCall]) -> IO ()
printAllCalls (c, g) = do
  putStrLnFgc Yellow "Direct calls:"
  printCalls c
  putStr "\n"
  putStrLnFgc Yellow "Group calls:"
  printGroupCalls g
