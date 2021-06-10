module GossipState where

import Data.HasCacBDD hiding ( evaluate, Top )
import Data.Map ( (!) )
import Data.Set ( (\\) )

import qualified Data.Set as Set

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