{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module GossipGraph where

-- ( AgentName,
--   AgentId,
--   Agent,
--   GossipGraph,
--   Relation,
--   Kind,
--   fromString,
--   fromAgentsAndRelations,
--   initialGraph
-- )

import Control.Arrow ((***))
import Control.Monad (join)
import qualified Data.Char as Char
import Data.Graph.Inductive (Gr, LEdge, LNode, prettyPrint, Graph (noNodes))
import Data.Graph.Inductive.Graph --(Graph (mkGraph), prettify, labNodes, hasLEdge)
import Data.List (find, filter)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe ()
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple (swap)

import GossipTypes

-- | Edge label indicating whether an agent knows the number or secret of
-- another agent
data Kind
  = Number
  | Secret
  deriving (Eq, Show)

type Relation = LEdge Kind
type GossipGraph = Gr AgentName Kind

printGraph :: GossipGraph -> IO ()
printGraph = prettyPrint

-- | Simple graph to be used for testing
testGraph :: GossipGraph
testGraph = initialGraph 3 [('a', ['a', 'b']), ('b', ['b', 'c']), ('c', ['c'])]

testGraph2 :: GossipGraph
testGraph2 = initialGraph 3 [('a',['a','b','c']), ('b',['b']), ('c',['c'])]

-- | Generate an initial gossip graph (with no initial shared secrets), based on a list of agents and their known phone numbers.
initialGraph :: Int -> [(Char, [Char])] -> GossipGraph
initialGraph nAgents numberLists =
  let agIds = [0 .. nAgents - 1]
      agLabs = map idToLab agIds

      nodes :: [Agent]
      nodes = zip agIds agLabs

      secrets :: [(Int, Int, Kind)]
      secrets = zip3 agIds agIds $ repeat Secret

      charmap :: Map Char Int
      charmap = Map.fromList $ map swap nodes

      flatten :: [(Char, [Char])] -> [(Char, Char)]
      flatten = foldr fun []
        where fun tup = (++) [(fst tup, x) | x <- snd tup]

      tupCharToInt :: (Char, Char) -> (Int, Int)
      tupCharToInt = join (***) (charmap !)

      withKind :: (Int, Int) -> (Int, Int, Kind)
      withKind tup = (fst tup, snd tup, Number)

      numbers :: [(Int, Int, Kind)]
      numbers = (map (withKind . tupCharToInt) . flatten) numberLists
   in mkGraph nodes (secrets ++ numbers)

-- | Convert an agent ID to an agent label
idToLab :: Int -> Char
idToLab = toEnum . (97 +)

-- | Convert an agent label to an agnet ID
labToId :: Char -> Int
labToId = flip (-) 97 . fromEnum

hasRelationWith :: GossipGraph -> Agent -> Kind -> Agent -> Bool
hasRelationWith g (ag1, _) kind (ag2, _) = hasLEdge g (ag1, ag2, kind)

numbersKnownBy :: GossipGraph -> Agent -> [Agent]
--numbersKnownBy graph agent = filter (hasRelationWith graph agent Number) (labNodes graph)
numbersKnownBy graph agent = map (agentFromId . fst) $ filter ((==) Number . snd) (lsuc graph $ fst agent)

secretsKnownBy :: GossipGraph -> Agent -> [Agent]
secretsKnownBy graph agent = map (agentFromId . fst) $ filter ((==) Secret . snd) (lsuc graph $ fst agent)

noAgents :: GossipGraph -> Int
noAgents = noNodes

-- | Warning, ignores the Char argument! Remains for legacy purposes. 
agent :: Int -> Char -> Agent
agent _id _ = agentFromId _id

agentFromId :: Int -> Agent
agentFromId id = (id, idToLab id)

agentFromLab :: Char -> Agent
agentFromLab lab = (labToId lab, lab)

relation :: Agent -> Agent -> Kind -> Relation
relation (from, _) (to, _) kind = (from, to, kind)

-- | Given a list of agents and a name, try to find a matching agent
findAgentByName :: [Agent] -> AgentName -> Maybe Agent
findAgentByName agents name =
  find (\a -> getCharName a == Char.toUpper name) agents
  where
    getCharName (_, n) = Char.toUpper n

-- | Check whether each agent is an expert; i.e. knows the secret of everyone.
graphIsComplete :: GossipGraph -> Bool
graphIsComplete g = length (edges g) == 2 * noAgents g ^ 2 --ufold ((&&) . isExpert (noAgents g)) True g

    -- isExpert :: Int -> ([(Kind, Node)], Node, AgentName, [(Kind, Node)]) -> Bool
    -- isExpert n c@(i, _, _, _) = length i == n * 2

-- === Parsing stuff below === --

-- | Possible tokens in the input string
data LexToken
  = Token Kind AgentName AgentId
  | Separator
  deriving (Eq, Show)

-- | Lexing for gossip graph input
lexer :: String -> Maybe [LexToken]
lexer input =
  charLexer 0 $ Text.unpack $ Text.strip $ Text.pack input
  where
    charLexer :: AgentId -> String -> Maybe [LexToken]
    charLexer _id chars =
      case chars of
        [] ->
          Just []
        c : cs ->
          if Char.isAlpha c
            then case charLexer _id cs of
              Just tokens ->
                if Char.isUpper c
                  then Just $ Token Secret c _id : tokens
                  else Just $ Token Number c _id : tokens
              Nothing ->
                Nothing
            else
              if c == ' '
                then case charLexer (_id + 1) cs of
                  Just tokens ->
                    Just $ Separator : tokens
                  Nothing ->
                    Nothing
                else Nothing

-- | Extract the agents from the lexed input string
parseAgents :: [LexToken] -> Maybe [Agent]
parseAgents tokens =
  parser tokens Set.empty Set.empty (-1) 1 1
    >>= validateNumberOfAgents
  where
    maybeAddName el acc =
      case el of
        Token _ n _ ->
          Set.insert (Char.toUpper n) acc
        _ ->
          acc

    agentNames = foldr maybeAddName Set.empty tokens

    numberOfSegments = (+ 1) $ length $ filter (== Separator) tokens

    validateNumberOfAgents :: [Agent] -> Maybe [Agent]
    validateNumberOfAgents agents
      -- No secrets, so ???
      | numberOfNames == 0 = Nothing
      -- More segments than agents
      | numberOfAgents < numberOfSegments = Nothing
      -- Different number of agents than names ???
      | numberOfAgents /= numberOfNames = Nothing
      -- All above is false, so input is correct
      | otherwise = Just agents
      where
        numberOfNames = Set.size agentNames
        numberOfAgents = length agents

    parser ::
      [LexToken] ->
      Set.Set AgentName ->
      Set.Set AgentName ->
      AgentId ->
      Int ->
      Int ->
      Maybe [Agent]
    parser ts segmentNames allNames highestIdAdded segmentStart pos =
      case ts of
        [] ->
          Just []
        token : rest ->
          case token of
            Token kind _name _id ->
              if Set.member ucName segmentNames
                then -- Duplicate agent name in a segment
                  Nothing
                else
                  if kind == Number
                    then -- number relation, so just add it to the list of segment names and continue

                      parser rest (Set.insert ucName segmentNames) allNames highestIdAdded segmentStart (pos + 1)
                        >>= Just
                    else -- secret relation
                      if _id > highestIdAdded && not (Set.member ucName allNames)
                        then -- an agent name we haven't seen before! add it to the list.

                          parser rest (Set.insert ucName segmentNames) (Set.insert ucName allNames) _id segmentStart (pos + 1)
                            >>= (\list -> Just (agent _id ucName : list))
                        else -- an agent name we have seen before. BORING! just continue.

                          parser rest (Set.insert ucName segmentNames) allNames highestIdAdded segmentStart (pos + 1)
                            >>= Just
              where
                ucName :: AgentName
                ucName = Char.toUpper _name
            Separator ->
              case head rest of
                Separator ->
                  Nothing
                _ ->
                  parser rest Set.empty allNames highestIdAdded (pos + 1) (pos + 1)

-- | Extract the relations from the parsed agents and the lexed input string
parseRelations :: [Agent] -> [LexToken] -> Maybe [Relation]
parseRelations agents tokens =
  case tokens of
    [] ->
      Just []
    Separator : rest ->
      parseRelations agents rest
    Token kind name _id : rest ->
      case parseRelations agents rest of
        Just relations ->
          case findAgentByName agents name of
            Just ag ->
              Just $ relation (agent _id name) ag kind : relations
            Nothing ->
              Nothing
        Nothing ->
          Nothing

-- | Try to parse a string representation of a gossip graph into a gossip graph
fromString :: String -> Maybe (Gr AgentName Kind)
fromString input =
  case (agents, relations) of
    (Just ag, Just rel) ->
      Just $ fromAgentsAndRelations ag rel
    _ ->
      Nothing
  where
    lexresult = lexer input
    agents = lexresult >>= parseAgents
    relations =
      case agents of
        Just ag ->
          lexresult >>= parseRelations ag
        _ ->
          Nothing

-- | Given a set of agents and corresponding relations, construct a gossip graph
fromAgentsAndRelations :: [Agent] -> [Relation] -> Gr AgentName Kind
fromAgentsAndRelations = mkGraph

-- | Debugging function to check if parsing was successful
toStringIfSuccessful :: Maybe (Gr Agent Relation) -> String
toStringIfSuccessful graph =
  case graph of
    Just g ->
      prettify g
    _ ->
      "Failed to generate a gossip graph"
