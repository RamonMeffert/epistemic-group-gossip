module GossipProtocol where

import Data.Maybe
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph

import GossipGraph

import GossipKnowledgeStructure


data State = State {
    stateGraph :: GossipGraph,
    stateKnowledgeStruct :: GossipKnowledgeStructure,
    stateCallSeq :: [Call]
}

-- | Placeholder (definitely needs updating!)
type GossipProtocol = GossipKnowledgeStructure -> Call -> Bool

-- | (from, to).
-- Call is considered to be bidirectional,
-- Optionally we could add a direction component to allow singe directional broadcasting calls.
type Call = (Agent, Agent)

-- | Perform call according to protocol:
    -- Determine what calls are accepted
    -- -> Choose the most appropriate call
    -- -> Execute call against the graph
    -- -> Return updated graph.
performProtocolTick :: GossipProtocol -> State -> State
performProtocolTick prot state =
    -- Needs implementation
    -- Figure something out to use >>=
    -- performCall graph $ preferredCall prot $ acceptedCalls graph
    flip makeCall state $ head $ selectedCalls prot state

-- | Determines based on the current GossipGraph state which calls are actually allowed to be made.
validCalls :: GossipGraph -> [Call]
validCalls g = [(x,y) | x@(i, _) <- labNodes g, y@(j, _) <- labNodes g, i /= j, hasLEdge g (i,j,Number)]

-- | Chooses the 'best' call using the rules stated in the protocol and the allowed calls.
    -- This might also be no call, hence Maybe.
selectedCalls :: GossipProtocol -> State -> [Call]
selectedCalls prot (State g k c) =
    filter (prot k) (validCalls g)

-- Ik ga maken:
-- User kan action intypen
--  -> 'maak een call' (wie belt 'char', wie wil je bellen [char])
--  -> 'mogelijke calls' (al dan niet gegeven een protocol)
--  -> 'wat is de state?' (printGraph . printSeq . printStruct)
--  -> 'export state' (poept LaTeX uit)
-- Er wordt gekeken of deze action toegestaan is
-- Indien ja 
--  -> update graph (in de State), append call aan callsequence
-- Indien nee -> vraag of de user actie wil forceren
-- Print nieuwe graph
-- Optioneel: print knowledge dingetjes

-- Optioneel:
-- Implementeer een simpel protocolletje
    -- -> Any (bel iedereen waarvan je nummer hebt, lekker gezelli)
    -- -> LearnNewSecrets (bel 1e de beste agent waarvan je wel nummer maar niet secret hebt)

makeCall :: Call -> State -> State
makeCall ((a, _), (b, _)) state = state { stateGraph = insEdge (a,b, Secret) (stateGraph state) }