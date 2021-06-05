module GossipProtocol where

import GossipGraph

-- | Placeholder
type GossipProtocol = Int

-- | Placeholder, possibly something similar (from, to, what)
type Action = (Agent, Agent, Relation)

performProtocolTick :: GossipProtocol -> GossipGraph -> GossipGraph
performProtocolTick prot graph =
    -- Perform action according to protocol
        -- Determine what actions are accepted
        -- Choose the mose appropriate action
        -- Execute action against the graph
        -- Return updated graph
    graph

acceptedActions :: GossipGraph -> [Action]
acceptedActions graph = []

preferredAction :: GossipProtocol -> [Action] -> Maybe Action
preferredAction prot [] = Nothing
preferredAction prot actions =
    -- To be implemented
    Just $ head actions