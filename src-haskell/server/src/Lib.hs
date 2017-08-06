module Lib
    ( runServer
    ) where

import Network.Socket
import System.IO

import Control.Monad (forM, forM_, void)
import Data.Aeson
import Data.Char
import Data.List (zip4)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8

import Types

runServer :: GameMap -> String -> Int -> IO ()
runServer gameMap port numberOfPlayers = withSocketsDo $ do
  addrinfos <- getAddrInfo
              (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
              Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock numberOfPlayers

  handles <- forM [1..numberOfPlayers] $ \_ -> do
    (connectionSocket, address) <- accept sock
    handle <- socketToHandle connectionSocket ReadWriteMode
    hSetBuffering handle LineBuffering
    return handle

  forM_ handles exchangeGreetings
  mapM_
    (uncurry4 doSetup)
    (zip4
      [0..]
      (repeat numberOfPlayers)
      (repeat gameMap)
      handles)

  forM_ handles hClose

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

doSetup :: Int -> Int -> GameMap -> Handle -> IO ()
doSetup punterId puntersCount gameMap handle = do
  let message = SSetupRq $ SetupRq punterId puntersCount gameMap
  sendMessage handle message
  void $ getMessage handle
