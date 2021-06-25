{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-|
Module      : GossipState
Description : Implements the Gossip state, a slice in time denoting the state of a gossip graph and knowledge structure.
Copyright   : (c) Jesper Kuiper, 2021
                  Leander van Boven, 2021
                  Ramon Meffert, 2021
License     : BSD3
-}
module GossipState 
  ( -- * State type
    State ( State, stateGraph, stateKnowledgeStruct, stateCallSeq )
  , printState
  -- * Call validation and execution
  , validCalls
  , makeCall
  , executeCalls
  -- * Epistemic formula evaluation
  , evaluateGossipAtom
  , evaluateBddVar
  , evaluate
  , (|=)
  -- * Legacy
  , evaluate'
  )
where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
--import Data.HasCacBDD hiding ( evaluate, Top )
import Data.Map.Strict ( (!) )
import Data.Set ( (\\) )
import System.Console.ANSI

import GossipGraph
import GossipKnowledge
import GossipTypes
import PrintableBdd hiding ( evaluate )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | The gossip state. 
data State = State {
  -- | The gossip graph of the current state.
  stateGraph :: GossipGraph,

  -- | The knowledge structure of the current state.
  stateKnowledgeStruct :: GossipKnowledgeStructure,

  -- | The call sequence of the current state. 
  stateCallSeq :: [Call]
}

-- | Prints the current state. 
printState :: State -> Bool -> IO ()
printState (State g k c) n = do
  setSGR [SetColor Foreground Vivid Magenta]
  if n
    then putStrLn "\n=== UPDATED STATE ==="
    else putStrLn "\n=== CURRENT STATE ==="
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "= GossipGraph ="
  setSGR [Reset]
  printGraph g
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "\n= Knowledgestructure ="
  setSGR [Reset]
  print k
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "\n= Callsequence ="
  setSGR [Reset]
  printCalls c
  setSGR [SetColor Foreground Vivid Magenta]
  putStrLn "\n====================="
  setSGR [Reset]

-- | Determines based on the current GossipGraph state which calls are actually allowed to be made. Note that no protocols are used for this, a call between a and b is valid iff a knows the number of b. 
validCalls :: GossipGraph -> ([Call], [GroupCall])
validCalls g = (validDirectCalls, validGroupCalls)
  where
    validDirectCalls :: [Call]
    validDirectCalls = [ f ☎ t | f@(i, _) <- labNodes g, t@(j, _) <- labNodes g, i /= j, hasLEdge g (i, j, Number)]

    validGroupCalls :: [GroupCall]
    validGroupCalls = [(f, tos f) | f@(i, _) <- labNodes g]

    tos :: Agent -> [Agent]
    tos f@(i, _) = [t | t@(j, _) <- labNodes g, f /= t, hasLEdge g (i,j,Number)]

-- | Makes multiple calls by repeatedly invoking `makeCall`. 
executeCalls :: [Call] -> State -> IO State
executeCalls [] s = return s
executeCalls (c:cs) s = do
  printMakeCall c
  let newState = makeCall c s
  printState newState True
  executeCalls cs newState

-- | Makes a call and update the state. 
makeCall :: Call -> State -> State
makeCall c s@(State g k cs) =
  newState s $ flip filter (allEdges c) $ not . hasLEdge g
  where
    allEdges :: Call -> [Relation]
    allEdges ((a, _), (b, _)) =
      -- (a,x,Number) forall x s.t. N(b,x)
      [(a, x, Number) | (x, _) <- labNodes g, hasLEdge g (b, x, Number)]
      ++
      -- (a,x,Secret) forall x s.t. S(b,x)
      [(a, x, Secret) | (x, _) <- labNodes g, hasLEdge g (b, x, Secret)]
      ++
      -- (b,x,Number) forall x s.t. N(a,x)
      [(b, x, Number) | (x, _) <- labNodes g, hasLEdge g (a, x, Number)]
      ++
      -- (b,x,Secret) forall x s.t. S(a,x)
      [(b, x, Secret) | (x, _) <- labNodes g, hasLEdge g (a, x, Secret)]

    newState :: State -> [Relation] -> State
    newState (State g k cs) newEdges = State
      (insEdges newEdges g)
      (updateWithCall k (length cs + 1) c)
      (cs ++ [c])

-- | Evaluates a gossip atom (N(x,y), S(x,y) or C(x,y)), given the current state
evaluateGossipAtom :: State -> GossipAtom -> Bool
evaluateGossipAtom (State g _ _) (GAt N x y) = hasRelationWith g x Number y
evaluateGossipAtom (State g _ _) (GAt S x y) = hasRelationWith g x Secret y
evaluateGossipAtom (State _ _ s) (GAt C x y) = (x, y) `elem` s

-- | Evaluates a Bdd variable as Int, given the current state
evaluateBddVar :: State -> Int -> Bool
evaluateBddVar s@(State g _ _) = evaluateGossipAtom s . intToGAt (noAgents g)

-- | Evaluates an epistemic formula, given the current state
evaluate :: State -> Form -> Bool
evaluate state@(State _ k _) ϕ = evaluateFun (k <|> ϕ) (evaluateBddVar state)

-- | An infix operator of the `evaluate` function. 
(|=) :: State -> Form -> Bool
state |= form = evaluate state form
infix 9 |=

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