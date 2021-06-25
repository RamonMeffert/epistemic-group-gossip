{-|
Module      : GossipProtocol
Description : Implements different kinds of (epistemic) protocols for dynamic gossip.
Copyright   : (c) Jesper Kuiper, 2021
                  Leander van Boven, 2021
                  Ramon Meffert, 2021
License     : BSD3
-}
module GossipProtocol 
  ( -- * Protocol type
    GossipProtocol
  , showProtocol
    -- * Different protocols
  , callAny
  , learnNewSecrets
  , possibleInformationGrowth
    -- * Protocol evaluation
  , selectedCalls 
  )

where

import Data.Map (keys, (!))
import Data.Maybe
import Data.Set ((\\), toList)
import System.Console.ANSI

import GossipKnowledge
import GossipGraph
import GossipState
import GossipTypes
import PrintableBdd


-- | A protocol for dynamic gossip. Following (van Ditmarsch et al., 2017), a gossip protocol is defined as an epistemic formula using two agents. If this formula resolves to true, a call between these agents is allowed. If the formula resolves to false, such a call is not allowed. The first argument denotes the amount of agents, the second argument is the current knowledge structure, whereas the third argument denotes the call in question. 
type GossipProtocol = Int -> GossipKnowledgeStructure -> Call -> Form

-- | Converts a protocol to a string notation, as used in (van Ditmarsch et al., 2017). 
showProtocol :: State -> GossipProtocol -> String
showProtocol (State g k s) prot = "φ(x,y) = " ++ show (prot 26 k (agentFromLab 'x',  agentFromLab 'y')) ++ " ∀x,y"

-- | The most simple and naive protocol, it allows any call to be made. φ(a,b) = ⊤
callAny :: GossipProtocol
callAny _ _ _ = Fact top

-- | This protocol only allows a call if it leads to new secrets to be learned. φ(a,b) = ¬S(a,b)
learnNewSecrets :: GossipProtocol
learnNewSecrets n _ (x, y) = Fact $ neg $ gAtToBdd n $ GAt S x y

-- | Introduced by (van Ditmarsch et al., 2017): "Call xy can be made if x knows y's number and if x considers it possible that there is a secret known by one of x,y but not the other." φ(a,b) = ⋁_{z ∈ A}( S(x, z) ↔︎ ¬S(y, z) )
possibleInformationGrowth :: GossipProtocol
possibleInformationGrowth n (GKS v t o) (x, y) = M x (Fact fullCond)
    where
        allAgents = keys o
        -- | S(x, z)
        c1 z = gAtToBdd n $ GAt S x z
        -- | ¬S(y, z)
        c2 z = neg $ gAtToBdd n $ GAt S y z
        -- | S(x, z) ↔︎ ¬S(y, z)
        cond z = c1 z `equ` c2 z
        -- | θ → ⋁_{z \in A}( S(x, z) ↔︎ ¬S(y, z) )
        fullCond = disSet $ map cond allAgents

-- | Selects all calls that are allowed by the given gossip protocol. Note that there could also be no calls allowed. 
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
