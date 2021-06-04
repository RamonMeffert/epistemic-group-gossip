module GossipKnowledgeStructure

where
import Data.Graph.Inductive.Graph
import Data.HasCacBDD
import Data.Map (Map)
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

import GossipGraph

data Rel = N | S | C deriving (Show, Ord, Eq, Enum)

data GossipAtom = GAt Rel Agent Agent deriving (Show, Ord, Eq)

-- | Converts Bdd variable to a GossipAtom
toGAt :: Int -> Bdd -> [GossipAtom]
toGAt n bdd = 
  let convert prp = GAt (toEnum rel) (a1, idToLab a1) (a2, idToLab a2)
        where rel =  prp        `div` (n^2)
              a1  = (prp - rel) `div`  n
              a2  = (prp - a1)
   in map convert (allVarsOf bdd)
      

-- | Convert a GossipAtom to a Bdd variable
fromGAt :: Int -> GossipAtom -> Bdd
fromGAt n (GAt rel (a1,_) (a2,_)) = var $ (n^2) * fromEnum rel + n * a1 + a2

-- data GossipAtom
--     = N Agent Agent                 -- ^ Agent x knows the number of agent y
--     | S Agent Agent                 -- ^ Agent x knows the secret of agent y
--     | C Agent Agent                 -- ^ Agent x has called agent y
--     deriving (Show, Ord, Eq) -- TODO derive Eq?

-- TODO: Is this necessary?
data GossipForm
    = Atom GossipAtom               -- ^ Atom
    | Neg GossipAtom                -- ^ Negation
    | Conj [GossipForm]             -- ^ Conjunction
    | Disj [GossipForm]             -- ^ Disjunction   
    | Impl GossipForm GossipForm    -- ^ Implication
    deriving (Show)

data GossipKnowledgeStructure = GKS
  { -- | The set of atoms available in the model
    vocabulary :: Set GossipAtom,

    -- | A boolean formula representing the law that every state needs to adhere to
    stateLaw :: Bdd,

    -- | The set of atoms seen by some agent
    observables :: Map Agent (Set GossipAtom)
  }

fromGossipGraph :: GossipGraph -> GossipKnowledgeStructure
fromGossipGraph graph =
  let agents = labNodes graph
      n = length agents

      -- vocabulary
      agentCombs = [(x,y) | x <- agents, y <- agents]
      vocab = foldr (\(x,y) -> (++) [GAt N x y, GAt S x y, GAt C x y]) [] agentCombs

      -- state law
      gvar = fromGAt n
      stateLaw = conSet 
        [ conSet [gvar (GAt S a a) | a <- agents] -- agents know their own secret
        , conSet [gvar (GAt N a a) | a <- agents] -- agents know their own number
        , conSet [gvar (GAt C x y) `imp` conSet   -- if x has called y...
            [ gvar (GAt N x y)                    --  then x knows y's number...
            , gvar (GAt S x y)                    --  and x knows y's secret...
            , gvar (GAt S y x)                    --  and y knows x's secret
            ]                                     --  TODO: Does y knows x's number?
          | x <- agents
          , y <- agents
          ]
        ]

      -- observables

   in undefined