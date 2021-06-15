module GossipKnowledge where

import Data.Graph.Inductive.Graph
--import Data.HasCacBDD
import Data.Map.Strict ( Map, unionWith, (!) )
import Data.Set ( Set, union, isSubsetOf, (\\) )
import System.IO

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import GossipGraph
import GossipTypes
import PrintableBdd hiding (var, substitSimul)
import Util


{- 
    Atomic propositions for gossip 
-}

-- | The relation type of a GossipAtom. 
data Rel = N  -- ^ x knows the number of y
         | S  -- ^ x knows the secret of y
         | C  -- ^ x has called y
         deriving (Show, Ord, Eq, Enum)

-- | An atomic proposition in a gossip state: Rel(Agent, Agent)
data GossipAtom = GAt Rel Agent Agent deriving (Ord, Eq)

instance Show GossipAtom where
  show (GAt rel a1 a2) = concat [show rel, showAgent a1, showAgent a2]

-- | Converts Bdd formula to a list of GossipAtoms for all variables in the formula
bddToGAt :: Int -> Bdd -> [GossipAtom]
bddToGAt n bdd = map (bddToGAt' n) (allVarsOf bdd)

-- | Converts a Bbd variable index to a GossipAtom
bddToGAt' :: Int -> Int -> GossipAtom
bddToGAt' n v = GAt (toEnum rel) (a1, idToLab a1) (a2, idToLab a2)
  where rel =  v                   `div` n^2
        a1  =  v `mod` n^2         `div` n
        a2  =  v `mod` n^2 `mod` n

-- | Convert a bdd variable index to a GossipAtom string
showBddVar :: Int -> Int -> String
showBddVar n = show . bddToGAt' n

var :: Int -> Int -> Bdd
var n = varl (showBddVar n)

-- | Convert a GossipAtom to a Bdd variable
gAtToBdd :: Int -> GossipAtom -> Bdd
gAtToBdd n gat = var n $ gAtToInt n gat

-- | Convert a GossipAtom to a unique integer
gAtToInt :: Int -> GossipAtom -> Int
gAtToInt n (GAt rel (a1,_) (a2,_)) = n^2 * fromEnum rel + n * a1 + a2

{- 
    Epistemic formulae for gossip
-}

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

instance Show Form where
  show (Fact bdd) = show bdd
  show (K ag form) = "K" ++ showAgent ag ++ " (" ++ show form ++ ")"


{- 
    Knowledge Structures for gossip
-}

-- | A knowledge structure (Gattinger, 2018) which represents the knowledge of a Gossip State
data GossipKnowledgeStructure = GKS
  { -- | The set of atoms available in the model
    vocabulary :: Set Int,

    -- | A boolean formula representing the law that every state needs to adhere to
    stateLaw :: Bdd,

    -- | The set of atoms seen by some agent
    observables :: Map Agent (Set Int)
  }

nag :: GossipKnowledgeStructure -> Int
nag (GKS _ _ o) = Map.size o

instance Show GossipKnowledgeStructure where
  show (GKS v l o) = concat
    [ "vocabulary: { ", List.intercalate ",\n              "  vocab, " }"
    , "\nstate law: " ++ show l
    , "\nobservables: ", List.intercalate "\n             " $ map shObs (Map.assocs o)
    ] where

      vocab = map (List.intercalate ", ") $ chunksOf 9 $ map gat (Set.toList v)

      gat :: Int -> String
      gat = show . bddToGAt' (Map.size o)

      shObs :: (Agent, Set Int) -> String
      shObs (a, s) =  concat
        [ showAgent a, " -> "
        , "{ ", List.intercalate ", " $ map gat (Set.toList s), " }"
        ]

      -- The following two functions were taken from the source code of Data.List.Split, so that we don't have to 
      -- import the entire module. 
      -- Source: https://hackage.haskell.org/package/split-0.2.3.4/docs/src/Data.List.Split.Internals.html#build
      build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
      build g = g (:) []

      chunksOf :: Int -> [e] -> [[e]]
      chunksOf i ls = map (take i) (build (splitter ls)) where
        splitter :: [e] -> ([e] -> a -> a) -> a -> a
        splitter [] _ n = n
        splitter l c n  = l `c` splitter (drop i l) c n

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
      stateLaw = con
        (conSet $ foldr (\ x -> (++) [gvar (GAt N x x), gvar (GAt S x x)]) [] agents)
        (conSet [gvar (GAt C x y) `imp` conSet   -- if x has called y;
            [ gvar (GAt N x y)                    --  then x knows y's number;
            , gvar (GAt N y x)                    --  and y knows x's number;
            , gvar (GAt S x y)                    --  and x knows y's secret;
            , gvar (GAt S y x)                    --  and y knows x's secret.
            ]
          | x <- agents
          , y <- agents
          , x /= y
          ]
        )

      -- observables
      initials = foldr (\ x -> (++) [GAt N x x, GAt S x x]) [] agents
      numbersOf ag = [GAt N ag x | x <- numbersKnownBy graph ag]

      observablesOf ag = map gint $ initials ++ numbersOf ag
      observables = Map.fromList [(a, Set.fromList $ observablesOf a) | a <- agents]

   in GKS (Set.fromList vocabulary) stateLaw observables

-- | Convert an epistemic formula to a boolean formula, given a knowledge structure
--   in Gattinger (2018), this is denoted by ||ϕ||_F
formToBdd :: GossipKnowledgeStructure -> Form -> Bdd
formToBdd _ (Fact bdd)  = bdd
formToBdd k (K ag form) = knowledgeToBdd k ag form
  where
    -- | Convert a knowledge formula (Ka ϕ) to a Bdd formula, given the current state
    knowledgeToBdd :: GossipKnowledgeStructure -> Agent -> Form -> Bdd
    knowledgeToBdd k ag form =
      let universe = vocabulary k \\ (observables k ! ag)
          formula = case form of
            Fact bdd   -> stateLaw k `imp` bdd
            K ag2 form -> stateLaw k `imp` knowledgeToBdd k ag2 form
      in boolQuant universe formula 

(<|>) :: GossipKnowledgeStructure -> Form -> Bdd
k <|> ϕ = formToBdd k ϕ
infix 9 <|>


{- 
    Knowledge transformer for gossip 
-}

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


{- 
    Update schemes for gossip calls
-}

synchronousUpdate :: GossipKnowledgeStructure -> (Agent, Agent) -> GossipKnowledgeStructure
synchronousUpdate gks (a, b) = gks |+| transformer
  where
        transformer = baseTransformer
          {  addObs = (Map.insert a oa . Map.insert b ob) Map.empty
          }
        
        oa = Set.fromList $ map (gAtToInt $ nag gks)
          [ GAt S a b
          , GAt S b a 
          , GAt N b a
          ]

        ob = Set.fromList $ map (gAtToInt $ nag gks)
          [ GAt S a b
          , GAt S b a
          , GAt N a b
          , GAt N b a
          ]