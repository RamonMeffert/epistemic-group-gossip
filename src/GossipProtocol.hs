module GossipProtocol where

import System.Console.ANSI

import GossipTypes
import GossipGraph
import GossipState
import GossipKnowledgeStructure

-- | Placeholder (definitely needs updating!)
type GossipProtocol = GossipKnowledgeStructure -> Call -> Bool

callAny :: GossipProtocol
callAny k c = True

learnNewSecrets :: GossipProtocol
learnNewSecrets k c = False

-- | Perform call according to protocol:
-- Determine what calls are accepted
-- -> Choose the most appropriate call
-- -> Execute call against the graph
-- -> Return updated graph.
performProtocolTick :: GossipProtocol -> State -> State
performProtocolTick prot state = flip makeCall state $ head $ selectedCalls prot state

-- | Chooses the 'best' call using the rules stated in the protocol and the allowed calls.
-- This might also be no call, hence Maybe.
selectedCalls :: GossipProtocol -> State -> [Call]
selectedCalls prot (State g k c) = filter (prot k) (validCalls g)
