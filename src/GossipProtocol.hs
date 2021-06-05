module GossipProtocol where

import GossipGraph

-- | Placeholder (definitely needs updating!)
type GossipProtocol = Int

-- | (from, to, what), Placeholder, possibly something similar in the end.
type Action = (Agent, Agent, Relation)

-- | Perform action according to protocol:
    -- Determine what actions are accepted
    -- -> Choose the most appropriate action
    -- -> Execute action against the graph
    -- -> Return updated graph.
performProtocolTick :: GossipProtocol -> GossipGraph -> GossipGraph
performProtocolTick prot graph =
    -- Needs implementation
    graph

-- | Determines based on the current GossipGraph state which calls are actually allowed to be made.
acceptedActions :: GossipGraph -> [Action]
acceptedActions graph = []

-- | Chooses the 'best' action using the rules stated in the protocol and the allowed actions.
    -- This might also be no action, hence Maybe.
preferredAction :: GossipProtocol -> [Action] -> Maybe Action
preferredAction prot actions =
    case actions of
        [] -> Nothing 
        actions -> Just $ head actions  -- To be implemented

-- | Intermediate function (might be merged with performProtocolTick later).
    -- Performs a graph state change if Just Action, otherwise returns the graph unchanged.
performAction :: GossipGraph -> Maybe Action -> GossipGraph 
performAction graph action = 
    case action of
        Nothing -> graph
        Just action -> graph    -- To be implemented