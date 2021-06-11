module GossipTypes where

import System.Console.ANSI
import Data.Graph.Inductive (Gr, LEdge, LNode, prettyPrint, Graph (noNodes))

type Agent = LNode Char
type AgentId = Int
type AgentName = Char

-- | (from, to).
-- Call is considered to be bidirectional,
-- Optionally we could add a direction component to allow singe directional broadcasting calls.
type Call = (Agent, Agent)

printCall :: Call -> IO ()
printCall ((_, i), (_, j)) = do
    putStr $ " (" ++ show i ++ "," ++ show j ++ ") "

printMakeCall :: Call -> IO ()
printMakeCall ((_,m), (_,n)) = do
    putStr "\nMaking call between "
    setSGR [SetColor Foreground Vivid Green]
    putStr $ show m
    setSGR [Reset]
    putStr " and "
    setSGR [SetColor Foreground Vivid Green]
    putStr $ show n

printCalls :: [Call] -> IO ()
printCalls [] = do
  putStr "["
  setSGR [SetColor Foreground Vivid Red]
  putStr "No calls to display" 
  setSGR [Reset]
  putStrLn "]"
printCalls c = mapM_ printCall c