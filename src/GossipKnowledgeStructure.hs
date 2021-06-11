module GossipKnowledgeStructure where

import Data.Graph.Inductive.Graph
import Data.HasCacBDD
import Data.Map (Map)
import Data.List

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
bddToGAt :: Int -> Bdd -> [GossipAtom]
bddToGAt n bdd = map (bddToGAt' n) (allVarsOf bdd)
      
-- | Converts a Bbd variable index to a GossipAtom
bddToGAt' :: Int -> Int -> GossipAtom
bddToGAt' n v = GAt (toEnum rel) (a1, idToLab a1) (a2, idToLab a2)
  where rel =  v        `div` (n^2)
        a1  = (v - rel) `div`  n
        a2  =  v - a1

-- | Convert a GossipAtom to a Bdd variable
gAtToBdd :: Int -> GossipAtom -> Bdd
gAtToBdd n gat = var $ gAtToInt n gat

-- | Convert a GossipAtom to a unique integer
gAtToInt :: Int -> GossipAtom -> Int 
gAtToInt n (GAt rel (a1,_) (a2,_)) = (n^2) * fromEnum rel + n * a1 + a2

-- | A formula build op of GossipAtoms, using the language of (van Ditmarsch et al., 2017).
--   We're not certain we need this, we might fully stick with Bdd formulae instead.
data GossipForm
    = Top                           -- ^ Always true
    | Atom GossipAtom               -- ^ Atom
    | Neg GossipForm                -- ^ Negation
    | Conj [GossipForm]             -- ^ Conjunction
    | Disj [GossipForm]             -- ^ Disjunction   
    | Impl GossipForm GossipForm    -- ^ Implication
    deriving (Show)

-- | An epistemic formula, defined in terms of Bdd's 
data Form = Fact Bdd 
          | K Agent Form

-- | A knowledge structure (Gattinger, 2018) which represents the knowledge of a Gossip State
data GossipKnowledgeStructure = GKS
  { -- | The set of atoms available in the model
    vocabulary :: [Int],

    -- | A boolean formula representing the law that every state needs to adhere to
    stateLaw :: Bdd,

    -- | The set of atoms seen by some agent
    observables :: Map Agent [Int]
  }

-- | Converts a gossip graph to its corresponding knowledge structure. 
--   Note that this doesn't convert a gossip state, i.e. no call sequence is encoded.
--   This is meant for the conversion of the _initial_ gossip state, (G, ε) 
--   (van Ditmarsch et al., 2017)
fromGossipGraph :: GossipGraph -> GossipKnowledgeStructure
fromGossipGraph graph =
  let agents = labNodes graph
      n = length agents

      gvar = gAtToBdd n
      gint = gAtToInt n

      -- vocabulary
      agentCombs = [(x,y) | x <- agents, y <- agents]
      vocabAtoms = foldr (\(x,y) -> (++) [GAt N x y, GAt S x y, GAt C x y]) [] agentCombs
      vocabulary = map gint vocabAtoms

      -- state law
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
      initialSecrets = [GAt S a a | a <- agents]
      numbersOf ag = [GAt N ag x | x <- numbersKnownBy graph ag]

      observablesOf ag = map gint $ initialSecrets ++ numbersOf ag
      observables = Map.fromList [(a, observablesOf a) | a <- agents]

   in GKS vocabulary stateLaw observables

synchronousUpdate :: GossipKnowledgeStructure -> (Agent, Agent) -> GossipKnowledgeStructure
synchronousUpdate gks call = gks { observables = newObs } 
  where
    newObs = undefined