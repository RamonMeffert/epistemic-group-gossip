module ToDo where

import System.Console.ANSI

getToDo :: IO ()
getToDo = do
    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "Program"
    setSGR [Reset]
    putStrLn "- Maak action: export state"
    putStrLn "- Als actie niet toegestaan: vraag of user actie wil forceren"
    putStrLn "- Zorg ervoor dat user op enter moet klikken om volgende protocoltick uit te voeren"

    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "GossipState"
    setSGR [Reset]
    putStrLn "- Implementeer action 'export state'"
    putStrLn "- Print knowledgestructure"

    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "GossipProtocol"
    setSGR [Reset]
    putStrLn "- Implementeer de 2 basis protocollen"