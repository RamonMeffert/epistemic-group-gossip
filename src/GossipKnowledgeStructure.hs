module GossipKnowledgeStructure
    (
    )
where
import Data.Map (Map)
import Data.Set (Set)

import GossipGraph

data GossipAtom
    = Top                           -- ^ Always true
    | N Agent Agent                 -- ^ Agent x knows the number of agent y
    | S Agent Agent                 -- ^ Agent x knows the secret of agent y
    | C Agent Agent                 -- ^ Agent x has called agent y
    deriving (Show) -- TODO derive Eq and/or Ord?

data GossipForm
    = Atom GossipAtom               -- ^ Atom
    | Neg GossipForm                -- ^ Negation
    | Conj [GossipForm]             -- ^ Conjunction
    | Disj [GossipForm]             -- ^ Disjunction   
    | Impl GossipForm GossipForm    -- ^ Implication
    deriving (Show)

data GossipKnowledgeStructure = GKS
  { -- | The set of atoms available in the model
    vocabulary :: Set GossipAtom,
    
    -- | A boolean formula representing the law that every state needs to adhere to
    stateLaw :: GossipForm,

    -- | The set of atoms seen by some agent
    observables :: Map Agent (Set GossipAtom)
  }


