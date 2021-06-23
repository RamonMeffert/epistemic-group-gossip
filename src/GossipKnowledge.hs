{-|
Module      : GossipKnowledge
Description : Implements Knowledge Structures and Knowledge Transformers as defined in (Gattinger, 2018).
Copyright   : (c) Jesper Kuiper, 2021
                  Leander van Boven, 2021
                  Ramon Meffert, 2021
License     : BSD3
-}

module GossipKnowledge 
  ( -- * Atomic proposition for Dynamic Gossip
    GossipAtom ( GAt )
  , Rel( N, S, C )
  -- ** Conversion to and from BDDs
  , intToGAt
  , bddToGAt
  , gAtToInt
  , gAtToBdd
  -- ** Texification
  , texifyGAt
  , texifyBddVar
  -- * Epistemic formulae for Dyamic Gossip
  , Form ( Fact, K )
  , GossipForm (Top, Atom, Neg, Conj, Disj, Impl )
  -- * Knowledge Structures for Dynamic Gossip
  , GossipKnowledgeStructure ( GKS )
  -- ** Knowledge Structure fields
  , vocabulary
  , stateLaw
  , observables
  -- ** Generation and validation
  , fromGossipGraph
  , validKS
  -- ** Transition from epistemic to boolean formulae
  , formToBdd
  , (<|>)
  -- * Knowledge Transformers for Dyamic Gossip
  , KnowledgeTransformer ( KT )
  -- ** Knowledge Transformer fields
  , addVocab
  , eventLaw
  , addObs
  -- ** Generation and validation
  , baseTransformer
  , validKT
  -- * Updating Knowledge Structures 
  , update
  , (|+|)
  , synchronousUpdate
  ) where

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
import PrintableBdd hiding ( var, forall, exists, forallSet, existsSet )
import Util


{- 
    Atomic propositions for gossip 
-}

-- | The relation type of a GossipAtom. 
data Rel = N  -- ^ Indicates that x knows the number of y.
         | S  -- ^ Indicates that x knows the secret of y.
         | C  -- ^ Indicates that x has called y.
         deriving (Show, Ord, Eq, Enum)

-- | An atomic proposition in a gossip state: Rel(Agent, Agent).
data GossipAtom = GAt Rel Agent Agent deriving (Ord, Eq)

instance Show GossipAtom where
  show (GAt rel a1 a2) = concat [show rel, showAgent a1, showAgent a2]

-- | Converts an atomic gossip proposition into a LaTeX string.
--
-- >>> texify (GAt N a b)
-- \texttt{N}ab
texifyGAt :: GossipAtom -> String
texifyGAt (GAt rel a1 a2) = List.concat ["\\texttt{", show rel, showAgent a1, showAgent a2, "}"]

-- | Converts a BDD formula to a list of GossipAtoms for all variables in the formula. Internally calls intToGAt.
--
-- >>> bddToGAt 3 ((var 0) `imp` (var 12))
-- >>> [Naa, Sba]
bddToGAt :: Int -> Bdd -> [GossipAtom]
bddToGAt n bdd = map (intToGAt n) (allVarsOf bdd)

-- | Converts a BDD variable index to a GossipAtom. Note that the first Int argument corresponds to the amount of agents present in the gossip graph. This is a bijective function to allow for unique indexing of atomic gossip propositions. 
--
-- >>> intToGAt 3 0
-- Naa
-- 
-- >>> intToGAt 3 12
-- Sba
intToGAt :: Int -> Int -> GossipAtom
intToGAt n v = GAt (toEnum rel) (a1, idToLab a1) (a2, idToLab a2)
  where rel =  v                   `div` n^2
        a1  =  v `mod` n^2         `div` n
        a2  =  v `mod` n^2 `mod` n

-- | Converts a BDD variable index to a GossipAtom, and converts that to a string.
--
-- >>> showBddVar 3 12
-- >>> "Sba"
showBddVar :: Int -> Int -> String
showBddVar n = show . intToGAt n

-- | Converts a BDD variable index to a GossipAtom, and converts that to a LaTeX string.
--
-- >>> texifyBddVar 3 12
-- "\texttt{S}ba"
texifyBddVar :: Int -> Int -> String
texifyBddVar n = texifyGAt . intToGAt n

-- | Converts a GossipAtom to a unique but labelled BDD variable. Note that the first argument corresponds to the amount of agents present in the gossip graph. 
--
-- >>> gAtToBdd 3 (GAt S b a)
-- Sba
--
-- Note that, while this function prints the normal gossip atom, the type that is returned is of BDD instead of GossipAtom. Internally, a unique variable index has been generated.
gAtToBdd :: Int -> GossipAtom -> Bdd
gAtToBdd n gat = gvar n $ gAtToInt n gat

-- | Converts a GossipAtom to a unique integer index that can be used for a BDD variable. 
--
-- >>> gAtToInt 3 (GAt S b a)
-- 12
gAtToInt :: Int -> GossipAtom -> Int
gAtToInt n (GAt rel (a1,_) (a2,_)) = n^2 * fromEnum rel + n * a1 + a2


-- Local helper functions

-- | Generates a labelled BDD variable based on agent count and a variable index. 
gvar :: Int -> Int -> Bdd
gvar n = varl (showBddVar n) (texifyBddVar n)

-- | The labeller that is used to label variables in the BDD. 
strLabel :: Int -> VarLabeller
strLabel = showBddVar

-- | The labeller that is used to label variables in the LaTeX string of the BDD.
texLabel :: Int -> VarLabeller
texLabel = texifyBddVar


{- 
    Epistemic formulae for gossip
-}

-- | A recursively defined propositional formula build out of GossipAtoms.
data GossipForm
    = Top                           -- ^ Always true
    | Atom GossipAtom               -- ^ Atom
    | Neg GossipForm                -- ^ Negation
    | Conj [GossipForm]             -- ^ Conjunction
    | Disj [GossipForm]             -- ^ Disjunction   
    | Impl GossipForm GossipForm    -- ^ Implication
    deriving (Show)

-- | An epistemic formula, defined in terms of BDDs. This data structure allows for propositional formulae, as well as (higher-order) agent knowledge.  
data Form = Fact Bdd
          | K Agent Form

instance Show Form where
  show (Fact bdd) = show bdd
  show (K ag form) = "K" ++ showAgent ag ++ show form


{- 
    Knowledge Structures for gossip
-}

-- | A knowledge structure (Gattinger, 2018) which represents the knowledge of a Gossip State. 
data GossipKnowledgeStructure = GKS
  { -- | The set of propositional atoms available in the model. 
    vocabulary :: Set Int,

    -- | A boolean formula representing the law that every state needs to adhere to. 
    stateLaw :: Bdd,

    -- | The set of atoms seen by some agent. If an atom is observable for an agent, then they are certain as to whether this atom is true or false. 
    observables :: Map Agent (Set Int)
  }

instance Show GossipKnowledgeStructure where
  show (GKS v l o) = concat
    [ "vocabulary: { ", List.intercalate ",\n              "  vocab, " }"
    , "\nstate law: " ++ show l
    , "\nobservables: ", List.intercalate "\n             " $ map shObs (Map.assocs o)
    ] where

      vocab = map (List.intercalate ", ") $ chunksOf 9 $ map gat (Set.toList v)

      gat :: Int -> String
      gat = show . intToGAt (Map.size o)

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
      -- End of copy

-- | Checks if a given Knowledge Structure is valid. It checks whether the variables found in both the state law and the observable sets are present in the vocabulary. 
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
      stateLaw = conSet (foldr (\ x -> (++) [gvar (GAt N x x), gvar (GAt S x x)]) [] agents)
        `con` conSet 
          [gvar (GAt C x y) `imp` conSet              --  if x has called y;
            [ gvar (GAt N x y)                        --  then x knows y's number;
            , gvar (GAt N y x)                        --  and y knows x's number;
            , gvar (GAt S x y)                        --  and x knows y's secret;
            , gvar (GAt S y x)                        --  and y knows x's secret;
            , gvar (GAt N x z) `imp` gvar (GAt N y z) --  and if x knows the number of z, then y also knows the number of z;
            , gvar (GAt N y z) `imp` gvar (GAt N x z) --  and if y knows the number of z, then x also knows the number of z;
            , gvar (GAt S x z) `imp` gvar (GAt S y z) --  and if x knows the secret of z, then y also knows the secret of z;
            , gvar (GAt S y z) `imp` gvar (GAt S x z) --  and if y knows the secret of z, then x also knows the secret of z.
            ]
          | x <- agents, y <- agents, x /= y, z <- agents, y /= z, x /= z] -- for all distinct x,y,z in A

      -- observables
      initials = foldr (\ x -> (++) [GAt N x x, GAt S x x]) [] agents
      numbersOf ag = [GAt N ag x | x <- numbersKnownBy graph ag]

      observablesOf ag = map gint $ initials ++ numbersOf ag
      observables = Map.fromList [(a, Set.fromList $ observablesOf a) | a <- agents]

   in GKS (Set.fromList vocabulary) stateLaw observables

-- | Converts an epistemic formula to a boolean formula, given a knowledge structure
--   in Gattinger (2018), `fromToBdd k ϕ` is denoted by ||ϕ||_k.
formToBdd :: GossipKnowledgeStructure -> Form -> Bdd
formToBdd _ (Fact bdd)  = bdd
formToBdd k (K ag form) = knowledgeToBdd k ag form
  where
    -- | Convert a knowledge formula (Ka ϕ) to a Bdd formula, given the current state.
    knowledgeToBdd :: GossipKnowledgeStructure -> Agent -> Form -> Bdd
    knowledgeToBdd k ag form =
      let universe = Set.toList $ vocabulary k \\ (observables k ! ag)
          formula = case form of
            Fact bdd   -> stateLaw k `imp` bdd
            K ag2 form -> stateLaw k `imp` knowledgeToBdd k ag2 form
      in gforallSet k universe formula

-- | An infix operator for the formToBdd function. 
(<|>) :: GossipKnowledgeStructure -> Form -> Bdd
k <|> ϕ = formToBdd k ϕ
infix 9 <|>


-- Private helper functions for Knowledge Structures

-- | Extracts the number of agents from the Knowledge Structure. 
nag :: GossipKnowledgeStructure -> Int
nag (GKS _ _ o) = Map.size o

gforall :: GossipKnowledgeStructure -> Int -> Bdd -> Bdd
gforall k = foralll (strLabel $ nag k) (texLabel $ nag k)

gexists :: GossipKnowledgeStructure -> Int -> Bdd -> Bdd
gexists k = existsl (strLabel $ nag k) (texLabel $ nag k)

gforallSet :: GossipKnowledgeStructure -> [Int] -> Bdd -> Bdd
gforallSet k = forallSetl (strLabel $ nag k) (texLabel $ nag k)

gexistsSet :: GossipKnowledgeStructure -> [Int] -> Bdd -> Bdd
gexistsSet k = existsSetl (strLabel $ nag k) (texLabel $ nag k)

{- 
    Knowledge transformer for gossip 
-}

-- | The Knowledge Transformer data structure, as defined in (Gattinger, 2018).
data KnowledgeTransformer = KT
  { -- | Additional vocabulary.
    addVocab :: Set Int,

    -- | The event law.
    eventLaw :: Bdd,

    -- | Additional observables.
    addObs :: Map Agent (Set Int)
  }

-- | A base Knowledge Transformer structure, which will not change the Knowledge Structure. For an arbitrary Knowledge Structure ks:
--
-- >>> ks |+| baseTransformer == ks
-- True
baseTransformer :: KnowledgeTransformer
baseTransformer = KT Set.empty top Map.empty

-- | Checks if a Knowledge Transformer is valid. 
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

synchronousUpdate :: GossipKnowledgeStructure -> Int -> Call -> GossipKnowledgeStructure
synchronousUpdate gks ticks (a, b) = gks |+| transformer
  where
        transformer = baseTransformer
          { addObs = (Map.insert a oa . Map.insert b ob) Map.empty
          --, eventLaw = xorSet $ map conSet allCallCombinations
          }

        agents = Map.keys $ observables gks
        --combinations = (. List.subsequences) . filter . (. length) . (==)
        --allCallCombinations = combinations ticks [gAtToBdd (nag gks) (GAt C x y) | x <- agents, y <- agents]

        oa = Set.fromList $ map (gAtToInt $ nag gks)
          [ GAt C a b
          , GAt S a b
          , GAt S b a
          , GAt N b a
          ]

        ob = Set.fromList $ map (gAtToInt $ nag gks)
          [ GAt C a b
          , GAt S a b
          , GAt S b a
          , GAt N a b
          , GAt N b a
          ]
