module GossipKnowledgeStructure where

import Data.Graph.Inductive.Graph
import Data.HasCacBDD
import Data.Map (Map)
import Data.Set (Set, union)

import qualified Data.Set as Set
import qualified Data.Map as Map

import GossipGraph

-- | The relation type of a GossipAtom. 
data Rel = N  -- ^ x knows the number of y
         | S  -- ^ x knows the secret of y
         | C  -- ^ x has called y
         deriving (Show, Ord, Eq, Enum)

-- | An atomic proposition in a gossip state: Rel(Agent, Agent)
data GossipAtom = GAt Rel Agent Agent deriving (Show, Ord, Eq)

-- | Converts Bdd formula to a list of GossipAtoms for all variables in the formula
toGAt :: Int -> Bdd -> [GossipAtom]
toGAt n bdd = map (toGAt' n) (allVarsOf bdd)
      
-- | Converts a Bbd variable index to a GossipAtom
toGAt' :: Int -> Int -> GossipAtom
toGAt' n v = GAt (toEnum rel) (a1, idToLab a1) (a2, idToLab a2)
  where rel =  v        `div` (n^2)
        a1  = (v - rel) `div`  n
        a2  =  v - a1

-- | Convert a GossipAtom to a Bdd variable
fromGAt :: Int -> GossipAtom -> Bdd
fromGAt n (GAt rel (a1,_) (a2,_)) = var $ (n^2) * fromEnum rel + n * a1 + a2

-- | A formula build op of GossipAtoms, using the language of (van Ditmarsch et al., 2017).
--   We're not certain we need this, we might fully stich with Bdd formulae instead.
data GossipForm
    = Top                           -- ^ Always true
    | Atom GossipAtom               -- ^ Atom
    | Neg GossipForm                -- ^ Negation
    | Conj [GossipForm]             -- ^ Conjunction
    | Disj [GossipForm]             -- ^ Disjunction   
    | Impl GossipForm GossipForm    -- ^ Implication
    | K Agent GossipAtom            -- ^ Knowledge
    deriving (Show)

-- | A knowledge structure (Gattinger, 2018) which represents the knowledge of a Gossip State
data GossipKnowledgeStructure = GKS
  { -- | The set of atoms available in the model
    vocabulary :: Set GossipAtom,

    -- | A boolean formula representing the law that every state needs to adhere to
    stateLaw :: Bdd,

    -- | The set of atoms seen by some agent
    observables :: Map Agent (Set GossipAtom)
  }

-- | Converts a gossip graph to its corresponding knowledge structure. 
--   Note that this doesn't convert a gossip state, i.e. no call sequence is encoded.
--   This is meant for the conversion of the _initial_ gossip state, (G, ε) 
--   (van Ditmarsch et al., 2017)
fromGossipGraph :: GossipGraph -> GossipKnowledgeStructure
fromGossipGraph graph =
  let agents = labNodes graph
      n = length agents

      -- vocabulary
      agentCombs = [(x,y) | x <- agents, y <- agents]
      vocab = foldr (\(x,y) -> (++) [GAt N x y, GAt S x y, GAt C x y]) [] agentCombs

      -- state law
      gvar :: GossipAtom -> Bdd
      gvar = fromGAt n

      stateLaw :: Bdd
      stateLaw = conSet 
        [ conSet [gvar (GAt S a a) | a <- agents] -- agents know their own secret
        , conSet [gvar (GAt N a a) | a <- agents] -- agents know their own number
        , conSet [gvar (GAt C x y) `imp` conSet   -- if x has called y;
            [ gvar (GAt N x y)                    --  then x knows y's number;
            , gvar (GAt N y x)                    --  and y knows x's number;
            , gvar (GAt S x y)                    --  and x knows y's secret;
            , gvar (GAt S y x)                    --  and y knows x's secret.
            ]
          | x <- agents
          , y <- agents
          ]
        ]

      -- observables
      initialSecrets :: Set GossipAtom
      initialSecrets = Set.fromList [GAt S a a | a <- agents]

      numbersOf :: Agent -> Set GossipAtom
      numbersOf ag = Set.fromList [GAt N ag x | x <- numbersKnownBy graph ag]

      observables :: Map Agent (Set GossipAtom)
      observables = Map.fromList [(a, initialSecrets `union` numbersOf a) | a <- agents]

   in GKS (Set.fromList vocab) stateLaw observables

synchronousUpdate :: GossipKnowledgeStructure -> (Agent, Agent) -> GossipKnowledgeStructure
synchronousUpdate gks call = gks { observables = newObs } 
  where
    newObs = undefined
    
    agentUpdate :: Set GossipAtom -> Set GossipAtom
    agentUpdate = undefined 