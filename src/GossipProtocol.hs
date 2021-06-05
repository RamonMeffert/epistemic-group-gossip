module GossipProtocol where

import GossipGraph

-- | Placeholder
type GossipProtocol = Int

performProtocolTick :: GossipProtocol -> GossipGraph -> GossipGraph
performProtocolTick prot graph =
    -- Perform action according to protocol
    graph