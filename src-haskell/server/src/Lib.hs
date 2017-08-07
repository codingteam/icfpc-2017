module Lib
    ( runServer
    ) where

import Network.Socket
import System.IO

import Algorithm.Search (dijkstra)
import Control.Monad (forM, forM_, void)
import Data.Aeson
import Data.Char
import Data.List (zip4, foldl')
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map.Strict as M

import Types

data GameState =
  GameState {
    gsRivers :: M.Map (SiteId, SiteId) (Maybe PunterId)
  , gsFutures :: M.Map PunterId [Future]
  }
  deriving (Show)

runServer :: GameMap -> String -> Int -> IO ()
runServer gameMap port numberOfPlayers = withSocketsDo $ do
  addrinfos <- getAddrInfo
              (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
              Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serveraddr)
  listen sock numberOfPlayers

  handles <- forM [1..numberOfPlayers] $ \_ -> do
    (connectionSocket, address) <- accept sock
    handle <- socketToHandle connectionSocket ReadWriteMode
    hSetBuffering handle LineBuffering
    return handle

  forM_ handles exchangeGreetings
  futuresList <- mapM
    (uncurry4 doSetup)
    (zip4
      [0..]
      (repeat numberOfPlayers)
      (repeat gameMap)
      handles)
  let futures = M.fromList futuresList

  let totalTurnsCount = length $ gmRivers gameMap
  let initialState = mkGameState gameMap futures
  (state, moves) <-
    playTurn
      numberOfPlayers
      handles
      totalTurnsCount
      (map Pass [0..numberOfPlayers-1])
      initialState

  let scores = map (calculatePunterScore gameMap state) [0..numberOfPlayers-1]
  -- send stop messages along with scores and last moves
  let formattedScores =
        map
          (\(p, s) -> Score p s)
          (zip [0..] scores)
  sendStop moves 0 handles formattedScores

  forM_ handles hClose

  print scores

calculatePunterScore :: GameMap -> GameState -> PunterId -> Int
calculatePunterScore gameMap state punterId =
  sum $
    (calculatePunterScoreForFutures gameMap state punterId)
    : map (calculatePunterScoreAtMine gameMap state punterId) (gmMines gameMap)

calculatePunterScoreForFutures :: GameMap -> GameState -> PunterId -> Int
calculatePunterScoreForFutures gameMap gs@(GameState state futures) punterId =
  case punterId `M.lookup` futures of
    Nothing -> 0
    Just futures' ->
      sum $ (flip map) futures' $ \(Future source target) ->
        if (hasPath gs punterId source target)
          then (distance gs source target) ^ 3
          else 0

calculatePunterScoreAtMine :: GameMap -> GameState -> PunterId -> SiteId -> Int
calculatePunterScoreAtMine gameMap state punterId mineId =
  sum $ map
        (calculateScoreForConnection state punterId mineId)
        (map siteId $ gmSites gameMap)

calculateScoreForConnection :: GameState -> PunterId -> SiteId -> SiteId -> Int
calculateScoreForConnection state punterId mineId siteId =
  if hasPath state punterId mineId siteId
    then (distance state mineId siteId) ^ 2
    else 0

hasPath :: GameState -> PunterId -> SiteId -> SiteId -> Bool
hasPath (GameState state _) punterId start finish =
  isJust $ dijkstra getNeighbours costFn (== finish) start
  where
  getNeighbours :: SiteId -> [SiteId]
  getNeighbours currentSiteId =
    let e = map fst $ filter ((== Just punterId) . snd) $ M.assocs state
        e_s = filter ((== currentSiteId) . fst) e
        e_t = filter ((== currentSiteId) . snd) e
    in (map fst e_t) ++ (map snd e_s)

  costFn :: SiteId -> SiteId -> Int
  costFn _ _ = 1

distance :: GameState -> SiteId -> SiteId -> Int
distance (GameState state _) start finish =
  case dijkstra getNeighbours costFn (== finish) start of
    Nothing -> 0
    Just (cost, _) -> cost
  where
  getNeighbours :: SiteId -> [SiteId]
  getNeighbours currentSiteId =
    let e = M.keys state
        e_s = filter ((== currentSiteId) . fst) e
        e_t = filter ((== currentSiteId) . snd) e
    in (map fst e_t) ++ (map snd e_s)

  costFn _ _ = 1

mkGameState :: GameMap -> M.Map PunterId [Future] -> GameState
mkGameState gameMap futures =
  let state = foldl'
        (\state river ->
          M.insert
            (source river, target river)
            Nothing
            state)
        M.empty
        (gmRivers gameMap)
  in GameState state futures

applyMove :: GameState -> Move -> GameState
applyMove state (Pass _) = state
applyMove (GameState s futures) (Claim p source target) =
  let s' = M.insert (source, target) (Just p) s
  in GameState s' futures

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (m, n, l, p) = f m n l p

getMessage :: Handle -> IO ClientMessage
getMessage handle = do
  len <- getMessageLength 0
  message <- B.hGet handle len
  let Just msg = decode message :: Maybe ClientMessage
  return msg

  where
  getMessageLength :: Int -> IO Int
  getMessageLength n = do
    char <- hGetChar handle
    if char == ':'
      then return n
      else getMessageLength (n*10 + (ord char - ord '0'))

sendMessage :: Handle -> ServerMessage -> IO ()
sendMessage handle message = do
  let serialized = encode message
  let len = C8.pack $ show $ B.length serialized
  B.hPut handle $ B.concat [ len, C8.singleton ':', serialized ]

exchangeGreetings :: Handle -> IO ()
exchangeGreetings handle = do
  greeting <- getMessage handle
  let CHelloRq hello = greeting
  let response = SHelloRs $ HelloRs $ hrqName hello
  sendMessage handle response

doSetup :: Int -> Int -> GameMap -> Handle -> IO (PunterId, [Future])
doSetup punterId puntersCount gameMap handle = do
  let settings = defaultSettings { sFutures = True }
  let message = SSetupRq $ SetupRq punterId puntersCount gameMap settings
  sendMessage handle message
  CSetupRs (SetupRs _ futures) <- getMessage handle
  return (punterId, futures)

playTurn :: Int -> [Handle] -> Int -> [Move] -> GameState -> IO (GameState, [Move])
playTurn _ _ 0 previousMoves state = return (state, previousMoves)
playTurn puntersCount handles n previousMoves state = do
  let currentIdx = n `mod` puntersCount
  let handle = handles !! currentIdx

  sendMessage handle (SMoveRq $ MoveRq previousMoves)
  CMove move <- getMessage handle
  let state' = applyMove state move
  let previousMoves' =
        (take currentIdx previousMoves)
        ++ [move]
        ++ (drop (currentIdx+1) previousMoves)

  playTurn puntersCount handles (n-1) previousMoves' state'

sendStop :: [Move] -> PunterId -> [Handle] -> [Score] -> IO ()
sendStop _            _        []          _          = return ()
sendStop moves@(m:ms) punterId (h:handles) scores = do
  let message = SStop $ StopRq moves scores
  sendMessage h message
  sendStop ((Pass(punterId)):ms) (punterId+1) handles scores
