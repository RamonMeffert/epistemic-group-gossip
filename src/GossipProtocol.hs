module GossipProtocol where

import System.Console.ANSI

import GossipTypes
import GossipGraph
import GossipState
import GossipKnowledge
import PrintableBdd

-- | Placeholder (definitely needs updating!)
type GossipProtocol = Int -> Call -> Form

callAny :: GossipProtocol
callAny n c = Fact top

learnNewSecrets :: GossipProtocol
learnNewSecrets n (x, y) = Fact $ neg $ gAtToBdd n $ GAt S x y

-- | Chooses the 'best' call using the rules stated in the protocol and the allowed calls.
-- This might also be no call, hence Maybe.
selectedCalls :: GossipProtocol -> State -> ([Call], [GroupCall])
selectedCalls prot s@(State g k c) =
  let valid = validCalls g
      direct = filter isSelected $ fst valid
      group = filter isGroupCallSelected $ snd valid
  in (direct, group)
  where
    isSelected :: Call -> Bool
    isSelected c = s |= prot (noAgents g) c

    isGroupCallSelected :: GroupCall -> Bool
    isGroupCallSelected g = all isSelected $ toCalls g
  