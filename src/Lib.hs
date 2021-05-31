module Lib
  ( someFunc,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HasCacBDD (Bdd)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Phantom type for agents.
newtype Agent = Agent Int

-- | Phantom type for atoms.
newtype Atom = Atom Char

-- | A knowledge structure represents a Kripke model
data KnowledgeStructure = KnowledgeStructure
  { -- | The set of atoms available in the model
    vocabulary :: Set Atom,
    -- | A boolean formula representing a boolean formula.
    -- TODO: This should be a BDD using HasCacBDD.
    stateLaw :: String,
    -- | The set of atoms seen by some agent
    observables :: Map Agent (Set Atom)
  }

-- | A scenario is a combination of a knowledge structure `F` and a state `s`,
-- where `s` is some subset of the vocabulary `V` of the model such that `s`
-- satisfies the state law.
data Scenario = Scenario KnowledgeStructure (Set Atom)