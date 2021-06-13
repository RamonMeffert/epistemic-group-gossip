module Util where

import System.Console.ANSI

printInvalidAction :: String -> IO ()
printInvalidAction action = do
    setSGR [SetColor Foreground Vivid Red]
    putStr "Invalid action: "
    setSGR [SetColor Foreground Vivid Blue]
    putStr action
    setSGR [Reset]
    putStr " is not a valid action in the current context.\n"

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