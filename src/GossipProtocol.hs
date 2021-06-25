module GossipProtocol where

import System.Console.ANSI

import GossipTypes
import GossipGraph
import GossipState
import GossipKnowledge
import Data.Map (keys, (!))
import Data.Set ((\\), toList)
import PrintableBdd
import Data.Maybe

-- | Placeholder (definitely needs updating!)
type GossipProtocol = GossipGraph -> GossipKnowledgeStructure -> Call -> Bool

callAny :: GossipProtocol
callAny _ k c = True

learnNewSecrets :: GossipProtocol
learnNewSecrets _ k c = False

-- | φ(x, y) := K̂_x ⋁_{z \in A}( S(x, z) ↔︎ ¬S(y, z) )
possibleInformationGrowth :: GossipProtocol
possibleInformationGrowth g k@(GKS v t o) (x, y) = fromMaybe False $ PrintableBdd.evaluate kCond assignments
    where
        allAgents = keys o
        n = length allAgents
        -- | V \ O_x
        relevantAtoms = v \\ (o ! x)
        -- | S(x, z)
        c1 z = gAtToBdd n $ GAt S x z
        -- | ¬S(y, z)
        c2 z = neg $ gAtToBdd n $ GAt S y z
        -- | S(x, z) ↔︎ ¬S(y, z)
        cond z = c1 z `equ` c2 z
        -- | θ → ⋁_{z \in A}( S(x, z) ↔︎ ¬S(y, z) )
        fullCond = imp t $ disSet $ map cond allAgents
        -- | Bdd equivalent to K̂_x ⋁_{z \in A}( S(x, z) ↔︎ ¬S(y, z) )
        kCond = neg $ conSet $ map (\a -> restrict fullCond (a, True) `con` restrict fullCond (a, False)) (toList relevantAtoms)
        -- | All assignments of S(x, y) for x, y in A
        assignments = [ (gAtToInt n (GAt S a b), hasRelationWith g a Secret b) | a <- allAgents, b <- allAgents]


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
selectedCalls prot (State g k c) = filter (prot g k) (validCalls g)
