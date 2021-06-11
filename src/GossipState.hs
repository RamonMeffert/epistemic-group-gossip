{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module GossipState where

import Data.HasCacBDD hiding ( evaluate, Top )
import Data.Map ( (!) )
import Data.List ( (\\) )

import qualified Data.Set as Set
import qualified Data.HasCacBDD as Bdd

import GossipGraph
import GossipKnowledgeStructure
import GossipProtocol hiding (State)

data State = State {
  stateGraph :: GossipGraph,
  stateKnowledgeStruct :: GossipKnowledgeStructure,
  stateCallSeq :: [Call]
}

printState :: State -> IO ()
printState (State g k c) = do
  putStrLn "= GossipGraph ="
  printGraph g
  putStrLn "= Knowledgestructure ="
  -- TODO: print knowledgestructure
  putStrLn "THIS HAS NOT BEEN IMPLEMENTED YET :("
  putStrLn "= Callsequence ="
  printCalls c

-- | Evaluate a gossip atom (N(x,y), S(x,y) or C(x,y)), given the current state
evaluateGossipAtom :: State -> GossipAtom -> Bool
evaluateGossipAtom (State g _ _) (GAt N x y) = hasRelationWith g x Number y
evaluateGossipAtom (State g _ _) (GAt S x y) = hasRelationWith g x Secret y
evaluateGossipAtom (State _ _ s) (GAt C x y) = (x, y) `elem` s

-- | Evaluate a Bdd variable as Int, given the current state
evaluateBddVar :: State -> Int -> Bool
evaluateBddVar s@(State g _ _) = evaluateGossipAtom s . bddToGAt' (noAgents g)

-- | Convert a knowledge formula (Ka φ) to a Bdd formula, given the current state
knowledgeToBdd :: State -> Agent -> Form -> Bdd
knowledgeToBdd s@(State _ k _) ag form =
  let universe = vocabulary k \\ (observables k ! ag)
      formula = case form of
        Fact bdd   -> stateLaw k `imp` bdd
        K ag2 form -> stateLaw k `imp` knowledgeToBdd s ag2 form

      boolQuant x = [restrict formula (x, True), restrict formula (x,False)]
   in conSet $ concatMap boolQuant universe

-- | Evaluate a formula, given the current state
evaluate :: State -> Form -> Bool
evaluate s (Fact bdd)  = evaluateFun  bdd                       (evaluateBddVar s)
evaluate s (K ag form) = evaluateFun (knowledgeToBdd s ag form) (evaluateBddVar s)

-- | Evaluate a formula, given the current state
(|=) :: State -> Form -> Bool
state |= form = evaluate state form

-- | Legacy method, evalute a GossipFormula (note, without knowledge) by naive recursion
evaluate' :: State -> GossipForm -> Bool
evaluate' _ Top                            = True
evaluate' (State g _ _) (Atom (GAt N x y)) = hasRelationWith g x Number y
evaluate' (State g _ _) (Atom (GAt S x y)) = hasRelationWith g x Secret y
evaluate' (State _ _ s) (Atom (GAt C x y)) = (x, y) `elem` s
evaluate' s (Neg form)                     = not $ evaluate' s form
evaluate' s (Conj (form:rest))             = evaluate' s form && evaluate' s (Conj rest)
evaluate' s (Disj (form:rest))             = evaluate' s form || evaluate' s (Disj rest)
evaluate' s (Impl prem conc)               = not $ evaluate' s prem || evaluate' s conc