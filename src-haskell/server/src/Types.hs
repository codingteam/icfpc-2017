{-# LANGUAGE OverloadedStrings #-}
module Types where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Aeson

type PunterName = T.Text
type PunterId = Int
type SiteId = Int

data ServerMessage =
  SHelloRs HelloRs
  | SSetupRq SetupRq
  | SMoveRq MoveRq
  | SStop StopRq
  deriving (Eq, Show)

instance FromJSON ServerMessage where
  parseJSON x@(Object v) = do
    if "you" `H.member` v
      then SHelloRs <$> parseJSON x
      else if "map" `H.member` v
        then SSetupRq <$> parseJSON x
        else if "move" `H.member` v
               then SMoveRq <$> parseJSON x
               else SStop <$> parseJSON x

instance ToJSON ServerMessage where
  toJSON (SHelloRs hello) = toJSON hello
  toJSON (SSetupRq setup) = toJSON setup
  toJSON (SMoveRq moves) = toJSON moves
  toJSON (SStop stop) = toJSON stop

data ClientMessage =
  CHelloRq HelloRq
  | CSetupRs SetupRs
  | CMove Move
  deriving (Eq, Show)

instance FromJSON ClientMessage where
  parseJSON x@(Object v) = do
    if "me" `H.member` v
      then CHelloRq <$> parseJSON x
      else if "ready" `H.member` v
        then CSetupRs <$> parseJSON x
        else CMove <$> parseJSON x

data HelloRq = HelloRq {hrqName :: PunterName}
  deriving (Eq, Show)

instance ToJSON HelloRq where
  toJSON (HelloRq name) = object ["me" .= name]

instance FromJSON HelloRq where
  parseJSON (Object v) = do
    name <- v .: "me"
    return $ HelloRq name

data HelloRs = HelloRs {hrsName :: PunterName}
  deriving (Eq, Show)

instance FromJSON HelloRs where
  parseJSON (Object v) = do
    name <- v .: "you"
    return $ HelloRs name

instance ToJSON HelloRs where
  toJSON (HelloRs name) = object ["you" .= name]

data Site = Site {siteId :: SiteId}
  deriving (Eq, Show)

instance FromJSON Site where
  parseJSON (Object v) = do
    id <- v .: "id"
    return $ Site id

instance ToJSON Site where
  toJSON (Site id) = object ["id" .= id]

data River = River {source :: SiteId, target :: SiteId}
  deriving (Eq, Show)

instance FromJSON River where
  parseJSON (Object v) = do
    src <- v .: "source"
    dst <- v .: "target"
    return $ River src dst

instance ToJSON River where
  toJSON (River source target) =
    object [
      "source" .= source
    , "target" .= target
    ]

data GameMap = GameMap {gmSites :: [Site], gmRivers :: [River], gmMines :: [SiteId]}
  deriving (Eq, Show)

instance FromJSON GameMap where
  parseJSON (Object v) = do
    sites <- v .: "sites"
    rivers <- v .: "rivers"
    mines <- v .: "mines"
    return $ GameMap sites rivers mines

instance ToJSON GameMap where
  toJSON (GameMap sites rivers mines) =
    object [
      "sites" .= sites
    , "rivers" .= rivers
    , "mines" .= mines
    ]

data SetupRq = SetupRq {srqPunter :: PunterId, srqPunters :: Int, srqMap :: GameMap}
  deriving (Eq, Show)

instance FromJSON SetupRq where
  parseJSON (Object v) = do
    punter <- v .: "punter"
    punters <- v .: "punters"
    map <- v .: "map"
    return $ SetupRq punter punters map

instance ToJSON SetupRq where
  toJSON (SetupRq id punters map) =
    object [
      "punter" .= id
    , "punters" .= punters
    , "map" .= map
    ]

data SetupRs = SetupRs {srsPunter :: PunterId}
  deriving (Eq, Show)

instance ToJSON SetupRs where
  toJSON (SetupRs punter) = object ["ready" .= punter]

instance FromJSON SetupRs where
  parseJSON (Object v) = do
    punterId <- v .: "ready"
    return $ SetupRs punterId

data Move =
  Claim {claimPunter :: PunterId, claimSource :: SiteId, claimTarget :: SiteId}
  | Pass {passPunter :: PunterId}
  deriving (Eq, Show)

instance FromJSON Move where
  parseJSON (Object v) = do
    mbPass <- v .:? "pass"
    case mbPass of
      Nothing -> do
        mbClaim <- v .:? "claim"
        case mbClaim of
          Nothing -> fail "invalid move"
          Just claim -> do
            punter <- claim .: "punter"
            source <- claim .: "source"
            target <- claim .: "target"
            return $ Claim punter source target
      Just pass -> do
        punter <- pass .: "punter"
        return $ Pass punter

instance ToJSON Move where
  toJSON (Pass punter) = object ["pass" .= object ["punter" .= punter]]

  toJSON (Claim punter source target) =
    object ["claim" .= object ["punter" .= punter, "source" .= source, "target" .= target]]

data MoveRq = MoveRq {mrqMoves :: [Move]}
  deriving (Eq, Show)

instance FromJSON MoveRq where
  parseJSON (Object v) = do
    move <- v .: "move"
    moves <- move .: "moves"
    return $ MoveRq moves

instance ToJSON MoveRq where
  toJSON (MoveRq moves) =
    object [
      "move" .= object [
        "moves" .= moves
      ]
    ]

type MoveRs = Move

data StopRq = StopRq {stopMoves :: [Move], stopScores :: [Score]}
  deriving (Eq, Show)

instance FromJSON StopRq where
  parseJSON (Object v) = do
    stop <- v .: "stop"
    moves <- stop .: "moves"
    return $ StopRq moves []

instance ToJSON StopRq where
  toJSON (StopRq moves scores) =
    object [
      "stop" .= object [
        "moves" .= moves
      , "scores" .= scores
      ]
    ]

data Score = Score {scorePunter :: PunterId, scoreValue :: Int}
  deriving (Eq, Show)

instance ToJSON Score where
  toJSON (Score punterId score) =
    object [
      "punter" .= punterId
    , "score" .= score
    ]
