{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module GossipGraph
  ( AgentName,
    AgentId,
    Agent,
    Relation,
    Kind,
    fromString,
    fromAgentsAndRelations,
  )
where

import Control.Monad (join)
import Control.Arrow ((***))
import qualified Data.Char as Char
import Data.Graph.Inductive (Gr)
import Data.Graph.Inductive.Graph (Graph (mkGraph), prettify)
import Data.List (find)
import Data.Map (Map, (!))
import Data.Maybe ()
import Data.Set (Set)
import Data.Tuple (swap)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Map as Map

type GossipGraph = Gr Char Kind

initialGraph :: [Char] -> Map Char [Char] -> GossipGraph
initialGraph agents numberLists = 
  let
    nodes = zip [0..] agents

    agIds = [0..length agents]
    secrets = zip3 agIds agIds $ repeat Secret

    mapTuple = join (***) (charmap !)
    charmap = Map.fromList $ map swap nodes
    -- TODO: convert number list to [Node, Node, Number]
    numbers = []
  in mkGraph nodes (secrets++numbers)

-- |
--    This part of the module is a simplified Haskell translation of <https://github.com/RamonMeffert/elm-gossip/blob/master/src/elm/GossipGraph/Parser.elm>

-- | Agent names
newtype AgentName = AgentName Char
  deriving (Eq, Ord, Show)

-- | Agent Ids
newtype AgentId = AgentId Int
  deriving (Num, Eq, Ord, Show)

-- | Agent
data Agent = Agent
  { a_name :: AgentName,
    a_id :: AgentId
  }
  deriving (Show)

-- | Gossip relation
data Relation = Relation
  { r_from :: AgentId,
    r_to :: AgentId,
    r_kind :: Kind
  }
  deriving (Show)

-- | Edge label indicating whether an agent knows the number or secret of
-- another agent
data Kind
  = Number
  | Secret
  deriving (Eq, Show)

-- | Given a list of agents and a name, try to find a matching agent
findAgentByName :: [Agent] -> AgentName -> Maybe Agent
findAgentByName agents (AgentName name) =
  find (\a -> getCharName a == Char.toUpper name) agents
  where
    getCharName (Agent (AgentName n) _) = Char.toUpper n

-- Parsing stuff below --

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
                  then Just $ Token Secret (AgentName c) _id : tokens
                  else Just $ Token Number (AgentName c) _id : tokens
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
        Token _ (AgentName n) _ ->
          Set.insert (Char.toUpper n) acc
        _ ->
          acc

    agentNames = foldr maybeAddName Set.empty tokens

    numberOfSegments = (+ 1) $ length $ filter (/= Separator) tokens

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
                    then -- lowercase character, so just add it to the list of segment names and continue

                      parser rest (Set.insert ucName segmentNames) allNames highestIdAdded segmentStart (pos + 1)
                        >>= Just
                    else
                      if _id > highestIdAdded && not (Set.member ucName allNames)
                        then -- an agent name we haven't seen before! add it to the list.

                          parser rest (Set.insert ucName segmentNames) (Set.insert ucName allNames) _id segmentStart (pos + 1)
                            >>= (\list -> Just (Agent ucName _id : list))
                        else -- an agent name we have seen before. BORING! just continue.

                          parser rest (Set.insert ucName segmentNames) allNames highestIdAdded segmentStart (pos + 1)
                            >>= Just
              where
                ucName :: AgentName
                ucName = (\(AgentName n) -> AgentName (Char.toUpper n)) _name
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
              Just $ Relation _id (a_id ag) kind : relations
            Nothing ->
              Nothing
        Nothing ->
          Nothing

-- | Try to parse a string representation of a gossip graph into a gossip graph
fromString :: String -> Maybe (Gr Agent Relation)
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
fromAgentsAndRelations :: [Agent] -> [Relation] -> Gr Agent Relation
fromAgentsAndRelations agents relations =
  mkGraph agentNodes relationEdges
  where
    agentNodes = map agToTup agents
    relationEdges = map relToTup relations

    agToTup (Agent n (AgentId i)) = (i, Agent n (AgentId i))
    relToTup (Relation (AgentId f) (AgentId t) k) = (f, t, Relation (AgentId f) (AgentId t) k)

-- | Debugging function to check if parsing was successful
toStringIfSuccessful :: Maybe (Gr Agent Relation) -> String
toStringIfSuccessful graph =
  case graph of
    Just g ->
      prettify g
    _ ->
      "Failed to generate a gossip graph"
