module Util where

import System.Console.ANSI

printInvalidActionStr :: String -> IO ()
printInvalidActionStr action = do
    putStrFgc Red "Invalid action: "
    putStrFgc Blue action
    putStrLn " is not a valid action in the current context."

printInvalidAction :: Char -> IO ()
printInvalidAction action = printInvalidActionStr [action]
    

printGraphComplete :: IO ()
printGraphComplete = do
    putStrLnFgc Green "\n!!! Graph complete !!!"
    putStrLn "Everybody is an expert (i.e. every agent knows the secrets of all other agents)."

putStrFgc :: Color -> String -> IO ()
putStrFgc c s = do
    setSGR [SetColor Foreground Vivid c] 
    putStr s
    setSGR [Reset]

putStrLnFgc :: Color -> String -> IO ()
putStrLnFgc c s = do
    setSGR [SetColor Foreground Vivid c] 
    putStrLn s
    setSGR [Reset]