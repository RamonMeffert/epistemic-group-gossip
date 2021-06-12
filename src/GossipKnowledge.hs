module GossipKnowledge where

import Data.Graph.Inductive.Graph
import Data.HasCacBDD
import Data.Map.Strict (Map, unionWith)
import Data.Set (Set, union, isSubsetOf)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import GossipGraph
import GossipTypes

-- | The relation type of a GossipAtom. 
data Rel = N  -- ^ x knows the number of y
         | S  -- ^ x knows the secret of y
         | C  -- ^ x has called y
         deriving (Show, Ord, Eq, Enum)

-- | An atomic proposition in a gossip state: Rel(Agent, Agent)
data GossipAtom = GAt Rel Agent Agent deriving (Ord, Eq)

instance Show GossipAtom where
  show (GAt rel a1 a2) = concat [show rel, "(", showAgent a1, ", ", showAgent a2, ")"]

-- | Converts Bdd formula to a list of GossipAtoms for all variables in the formula
bddToGAt :: Int -> Bdd -> [GossipAtom]
bddToGAt n bdd = map (bddToGAt' n) (allVarsOf bdd)

-- | Converts a Bbd variable index to a GossipAtom
bddToGAt' :: Int -> Int -> GossipAtom
bddToGAt' n v = GAt (toEnum rel) (a1, idToLab a1) (a2, idToLab a2)
  where rel =  v                   `div` n^2
        a1  =  v `mod` n^2         `div` n
        a2  =  v `mod` n^2 `mod` n 

-- | Convert a GossipAtom to a Bdd variable
gAtToBdd :: Int -> GossipAtom -> Bdd
gAtToBdd n gat = var $ gAtToInt n gat

-- | Convert a GossipAtom to a unique integer
gAtToInt :: Int -> GossipAtom -> Int
gAtToInt n (GAt rel (a1,_) (a2,_)) = n^2 * fromEnum rel + n * a1 + a2

-- | Convert a bdd variable index to a GossipAtom string
showBddVar :: Int -> Int -> String
showBddVar n = show . bddToGAt' n

-- | A formula build o' of GossipAtoms, using the language of (van Ditmarsch et al., 2017).
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
    vocabulary :: Set Int,

    -- | A boolean formula representing the law that every state needs to adhere to
    stateLaw :: Bdd,

    -- | The set of atoms seen by some agent
    observables :: Map Agent (Set Int)
  }

validKS :: GossipKnowledgeStructure -> Bool
validKS (GKS vocab law obs) =
  let lawCheck = Set.fromList (allVarsOf law) `isSubsetOf` vocab
      obsCheck = Map.foldr ((&&) . (`isSubsetOf` vocab)) True obs
   in lawCheck && obsCheck

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
      observables = Map.fromList [(a, Set.fromList $ observablesOf a) | a <- agents]

   in GKS (Set.fromList vocabulary) stateLaw observables

data KnowledgeTransformer = KT
  { -- | Additional vocabulary
    addVocab :: Set Int,

    -- | Event law
    eventLaw :: Bdd,

    -- | Additional observables
    addObs :: Map Agent (Set Int)
  }

baseTransformer :: KnowledgeTransformer
baseTransformer = KT Set.empty top Map.empty

validKT :: GossipKnowledgeStructure -> KnowledgeTransformer -> Bool
validKT (GKS v _ o) (KT v' l' o') =
  let vocabCheck = Set.disjoint v v'
      lawCheck = Set.fromList (allVarsOf l') `isSubsetOf` (v `union` v')
      obsCheck = Map.foldr ((&&) . (`isSubsetOf` v')) True o'
   in vocabCheck && lawCheck && obsCheck

update :: GossipKnowledgeStructure -> KnowledgeTransformer -> GossipKnowledgeStructure
update (GKS v l o) (KT v' l' o') = GKS
  { vocabulary = v `union` v',
    stateLaw = l `con` l',
    observables = unionWith union o o'
  }

(|+|) :: GossipKnowledgeStructure -> KnowledgeTransformer -> GossipKnowledgeStructure
f |+| x = update f x
infixl 9 |+|

synchronousUpdate :: GossipKnowledgeStructure -> (Agent, Agent) -> GossipKnowledgeStructure
synchronousUpdate gks (ag1, ag2) = gks |+| transformer
  where
        transformer = baseTransformer
          { addObs = undefined
          }