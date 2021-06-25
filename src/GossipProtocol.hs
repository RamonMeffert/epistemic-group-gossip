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
type GossipProtocol = Int -> GossipKnowledgeStructure -> Call -> Form

callAny :: GossipProtocol
callAny n _ c = Fact top

learnNewSecrets :: GossipProtocol
learnNewSecrets n _ (x, y) = Fact $ neg $ gAtToBdd n $ GAt S x y

possibleInformationGrowth :: GossipProtocol
possibleInformationGrowth n (GKS v t o) (x, y) = M x (Fact fullCond)
    where
        allAgents = keys o
        -- | V \ O_x
        relevantAtoms = v \\ (o ! x)
        -- | S(x, z)
        c1 z = gAtToBdd n $ GAt S x z
        -- | ¬S(y, z)
        c2 z = neg $ gAtToBdd n $ GAt S y z
        -- | S(x, z) ↔︎ ¬S(y, z)
        cond z = c1 z `equ` c2 z
        -- | θ → ⋁_{z \in A}( S(x, z) ↔︎ ¬S(y, z) )
        fullCond = disSet $ map cond allAgents

-- | Chooses the 'best' call using the rules stated in the protocol and the allowed calls.
-- This might also be no call, hence Maybe.
selectedCalls :: GossipProtocol -> State -> ([Call], [GroupCall])
selectedCalls prot s@(State g k c) =
  let valid = validCalls g
      direct = filter isSelected $ fst valid
      group = filter (\ g -> isGroupCallSelected g && isActualGroup g) (snd valid)
  in (direct, group)
  where
    isSelected :: Call -> Bool
    isSelected c = s |= prot (noAgents g) k c

    isGroupCallSelected :: GroupCall -> Bool
    isGroupCallSelected g = all isSelected $ toCalls g

    isActualGroup :: GroupCall -> Bool
    isActualGroup (_, []) = False
    isActualGroup (_, h:t) = not $ null t