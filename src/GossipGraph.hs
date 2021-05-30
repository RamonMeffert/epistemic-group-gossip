{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GossipGraph where

import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Set  as Set
import Data.List
import Data.Maybe
import Data.Graph.Inductive.Graph (Graph(mkGraph), DynGraph, prettify)
import Data.Graph.Inductive (Gr)


-- | Phantom type for agents.
newtype AgentName = AgentName Char
    deriving (Eq, Ord, Show)

newtype AgentId = AgentId Int
    deriving (Num, Eq, Ord, Show)

data Agent = Agent
    { a_name :: AgentName
    , a_id :: AgentId
    }
    deriving (Show)

data Relation = Relation
    { r_from :: AgentId
    , r_to   :: AgentId
    , r_kind :: Kind
    }
    deriving (Show)

-- | Edge label indicating whether an agent knows the number or secret of
-- another agent
data Kind 
    = Number 
    | Secret
    deriving (Eq, Show)


findAgentByName :: [Agent] -> AgentName -> Maybe Agent
findAgentByName agents (AgentName name) =
    find (\a -> getCharName a == Char.toUpper name) agents
    where
        getCharName (Agent (AgentName n) _) = Char.toUpper n


-- Parsing stuff below --

data LexToken
    = Token Kind AgentName AgentId
    | Separator
    deriving (Show)

-- Lexing for gossip graph input
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
                    if Char.isAlpha c then
                        case charLexer _id cs of
                            Just tokens ->
                                if Char.isUpper c then
                                    Just $ Token Secret (AgentName c) _id : tokens
                                else
                                    Just $ Token Number (AgentName c) _id : tokens

                            Nothing ->
                                Nothing
                    else if c == ' ' then
                        case charLexer (_id + 1) cs of 
                            Just tokens ->
                                Just $ Separator : tokens
                            
                            Nothing ->
                                Nothing
                    else
                        Nothing


parseAgents :: [LexToken] -> Maybe [Agent]
parseAgents tokens =
    parser tokens Set.empty Set.empty (-1) 1 1
    where
        parser :: [LexToken] 
            -> Set.Set AgentName
            -> Set.Set AgentName
            -> AgentId
            -> Int
            -> Int
            -> Maybe [Agent]
        parser ts segmentNames allNames highestIdAdded segmentStart pos =
            case ts of
                [] ->
                    Just []

                token : rest ->
                    case token of
                        Token kind _name _id ->
                            if Set.member ucName segmentNames then
                                -- Duplicate agent name in a segment
                                Nothing

                            else if kind == Number then
                                -- lowercase character, so just add it to the list of segment names and continue
                                parser rest (Set.insert ucName segmentNames) allNames highestIdAdded segmentStart (pos + 1)
                                    >>= Just

                            else if _id > highestIdAdded && not (Set.member ucName allNames) then
                                -- an agent name we haven't seen before! add it to the list.
                                parser rest (Set.insert ucName segmentNames) (Set.insert ucName allNames) _id segmentStart (pos + 1)
                                    >>= (\list -> Just (Agent ucName _id : list))

                            else
                                -- an agent name we have seen before. BORING! just continue.
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


toStringIfSuccessful :: Maybe (Gr Agent Relation) -> String
toStringIfSuccessful graph =
    case graph of
        Just g ->
            prettify g
        
        _ ->
            "Failed to generate a gossip graph"


fromAgentsAndRelations :: [Agent] -> [Relation] -> Gr Agent Relation
fromAgentsAndRelations agents relations =
    mkGraph agentNodes relationEdges
    where
        agentNodes = map agToTup agents
        relationEdges = map relToTup relations

        agToTup (Agent n (AgentId i)) = (i, Agent n (AgentId i))
        relToTup (Relation (AgentId f) (AgentId t) k) = (f, t, Relation (AgentId f) (AgentId t) k)