{-|
Module      : GossipProtocol
Description : Implements some utility functions that are used throughout the implementation.
Copyright   : (c) Jesper Kuiper, 2021
                  Leander van Boven, 2021
                  Ramon Meffert, 2021
License     : BSD3
-}

module Util 
  ( -- * Coloured printing
    putStrFgc
  , putStrLnFgc
  -- * Messaging
  , printInvalidAction
  , printGraphComplete
  -- * Pretty BDD to graph saving
  , writeToSvgGraph
  )
where

import System.Console.ANSI
import System.IO
import PrintableBdd
import Data.HasCacBDD.Visuals

-- | Prints a message that an action is invalid for the current context. 
printInvalidAction :: Char -> IO ()
printInvalidAction action = do
  putStrFgc Red "Invalid action: "
  putStrFgc Blue [action]
  putStrLn " is not a valid action in the current context."

-- | Prints a message that the current graph is complete, i.e. every agent is an expert. 
printGraphComplete :: IO ()
printGraphComplete = do
  putStrLnFgc Green "\n!!! Graph complete !!!"
  putStrLn "Everybody is an expert (i.e. every agent knows the secrets of all other agents)."

-- | Prints the given message in a given colour. 
putStrFgc :: Color -> String -> IO ()
putStrFgc c s = do
  setSGR [SetColor Foreground Vivid c]
  putStr s
  setSGR [Reset]

-- | Prints the given mesage with a new line in a given colour. 
putStrLnFgc :: Color -> String -> IO ()
putStrLnFgc c s = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn s
  setSGR [Reset]

-- | Save a Bdd in its graph structure as an SVG file. Uses the dot language, for which Graphviz needs to be installed. 
writeToSvgGraph :: String -> Bdd -> IO ()
writeToSvgGraph s b = svgGraph (bdd b) >>= writeFile s