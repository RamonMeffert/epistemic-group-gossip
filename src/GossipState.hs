module GossipState where

import Data.HasCacBDD hiding ( evaluate, Top )
import Data.Map ( (!) )
import Data.Set ( (\\) )

import qualified Data.Set as Set
import System.Console.ANSI

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph

import GossipTypes
import GossipGraph
import GossipKnowledgeStructure

data State = State {
  stateGraph :: GossipGraph,
  stateKnowledgeStruct :: GossipKnowledgeStructure,
  stateCallSeq :: [Call]
}

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
  putStrLn "THIS HAS NOT BEEN IMPLEMENTED YET :("
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "\n= Callsequence ="
  setSGR [Reset]
  printCalls c
  setSGR [SetColor Foreground Vivid Magenta]
  putStrLn "\n==="
  setSGR [Reset]

-- | Determines based on the current GossipGraph state which calls are actually allowed to be made.
validCalls :: GossipGraph -> [Call]
validCalls g = [(x, y) | x@(i, _) <- labNodes g, y@(j, _) <- labNodes g, i /= j, hasLEdge g (i, j, Number)]

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
    newState (State g k cs) newEdges = State (insEdges newEdges g) k (cs ++ [c])

-- Todo: Make evaluate function in terms of Bdd's instead of own 
evaluate :: State -> GossipForm -> Bool
evaluate _ Top = True
evaluate (State g _ _) (Atom (GAt N x y))            = hasRelationWith g x Number y
evaluate (State g _ _) (Atom (GAt S x y))            = hasRelationWith g x Secret y
evaluate (State _ _ s) (Atom (GAt C x y))            = (x, y) `elem` s
evaluate s (Neg form)                                = not $ evaluate s form
evaluate s (Conj (form:rest))                        = evaluate s form && evaluate s (Conj rest)
evaluate s (Disj (form:rest))                        = evaluate s form || evaluate s (Disj rest)
evaluate s (Impl prem conc)                          = not $ evaluate s prem || evaluate s conc
evaluate s@(State g (GKS voc law obs) _) (K ag form) = undefined
  where
    thetas = map (fromGAt $ noAgents g) $ Set.toList $ voc \\ (obs ! ag)
    forms = map (law `con`) thetas

(|=) :: State -> GossipForm -> Bool
state |= form = evaluate state form