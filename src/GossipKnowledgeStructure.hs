module GossipKnowledgeStructure

where
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Set as Set
import qualified Data.Map as Map

import GossipGraph ( GossipGraph )

type Agent = Char

data GossipAtom
    = Top                           -- ^ Always true
    | N Agent Agent                 -- ^ Agent x knows the number of agent y
    | S Agent Agent                 -- ^ Agent x knows the secret of agent y
    | C Agent Agent                 -- ^ Agent x has called agent y
    | Neg GossipAtom                -- ^ Negation
    deriving (Show, Ord, Eq) -- TODO derive Eq?

data GossipForm
    = Atom GossipAtom               -- ^ Atom
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

fromGossipGraph :: GossipGraph -> GossipKnowledgeStructure
fromGossipGraph graph = 
  let 
    agents = Graph.labNodes graph
  in undefined