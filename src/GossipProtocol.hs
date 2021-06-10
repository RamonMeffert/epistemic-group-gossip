module GossipProtocol where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph

import GossipGraph

import GossipKnowledgeStructure



data State = State {
    stateGraph :: GossipGraph,
    stateKnowledgeStruct :: GossipKnowledgeStructure,
    stateCallSeq :: [Call]
}

printState :: State -> IO ()
printState (State g k c) = do
    putStrLn "= GossipGraph ="
    printGraph g
    putStrLn "= Knowledgestructure ="
    -- TODO: print knowledgestructure
    putStrLn "THIS HAS NOT BEEN IMPLEMENTED YET :("
    putStrLn "= Callsequence ="
    printCalls c

-- | Placeholder (definitely needs updating!)
type GossipProtocol = GossipKnowledgeStructure -> Call -> Bool

-- | (from, to).
-- Call is considered to be bidirectional,
-- Optionally we could add a direction component to allow singe directional broadcasting calls.
type Call = (Agent, Agent)

printCalls :: [Call] -> IO ()
printCalls [] = putStrLn "No calls to display"
printCalls c = print c

-- | Perform call according to protocol:
    -- Determine what calls are accepted
    -- -> Choose the most appropriate call
    -- -> Execute call against the graph
    -- -> Return updated graph.
performProtocolTick :: GossipProtocol -> State -> State
performProtocolTick prot state = flip makeCall state $ head $ selectedCalls prot state

-- | Determines based on the current GossipGraph state which calls are actually allowed to be made.
validCalls :: GossipGraph -> [Call]
validCalls g = [(x,y) | x@(i, _) <- labNodes g, y@(j, _) <- labNodes g, i /= j, hasLEdge g (i,j,Number)]

-- | Chooses the 'best' call using the rules stated in the protocol and the allowed calls.
    -- This might also be no call, hence Maybe.
selectedCalls :: GossipProtocol -> State -> [Call]
selectedCalls prot (State g k c) = filter (prot k) (validCalls g)

-- Ik ga maken:
-- User kan action intypen
--  -> 'maak een call' (wie belt 'char', wie wil je bellen [char])
--  -> 'mogelijke calls' (al dan niet gegeven een protocol)
--  -> 'wat is de state?' (printGraph . printSeq . printStruct)
--  -> TODO: 'export state' (poept LaTeX uit)
-- Er wordt gekeken of deze action toegestaan is
-- Indien ja 
--  -> update graph (in de State), append call aan callsequence
-- TODO: Indien nee -> vraag of de user actie wil forceren?
-- Print nieuwe graph
-- Optioneel: print knowledge dingetjes

-- Optioneel:
-- Implementeer een simpel protocolletje
    -- -> Any (bel iedereen waarvan je nummer hebt, lekker gezelli)
    -- -> LearnNewSecrets (bel 1e de beste agent waarvan je wel nummer maar niet secret hebt)

makeCall :: Call -> State -> State
makeCall c s@(State g k cs) =
    newState s $ flip filter (allEdges c) $ not . hasLEdge g
    where
        allEdges :: Call -> [LEdge Kind]
        allEdges ((a, _), (b, _)) =
            -- (a,x,Number) forall x s.t. N(b,x)
            [(a,x,Number) | (x,_) <- labNodes g, hasLEdge g (b,x,Number)] ++
            -- (a,x,Secret) forall x s.t. S(b,x)
            [(a,x,Secret) | (x,_) <- labNodes g, hasLEdge g (b,x,Secret)] ++
            -- (b,x,Number) forall x s.t. N(a,x)
            [(b,x,Number) | (x,_) <- labNodes g, hasLEdge g (a,x,Number)] ++
            -- (b,x,Secret) forall x s.t. S(a,x)
            [(b,x,Secret) | (x,_) <- labNodes g, hasLEdge g (a,x,Secret)]

        newState :: State -> [LEdge Kind] -> State
        newState (State g k cs) newEdges = State (insEdges newEdges g) k (cs ++ [c])
