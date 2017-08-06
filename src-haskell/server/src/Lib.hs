module Lib
    ( runServer
    ) where

import Network.Socket
import System.IO

import Control.Monad (forM, forM_)
import Data.Aeson
import Data.Char
import qualified Data.ByteString.Lazy as B

import Types

runServer :: GameMap -> String -> Int -> IO ()
runServer map port numberOfPlayers = withSocketsDo $ do
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

  forM_ handles hClose

exchangeGreetings :: Handle -> IO ()
exchangeGreetings handle = do
  len <- getMessageLength 0
  message <- B.hGet handle len
  let Just greeting = decode message :: Maybe ClientMessage
  let CHelloRq hello = greeting
  let response = SHelloRs $ HelloRs $ hrqName hello
  B.hPut handle (encode response)

  where
  getMessageLength :: Int -> IO Int
  getMessageLength n = do
    char <- hGetChar handle
    if char == ':'
      then return n
      else getMessageLength (n*10 + (ord char - ord '0'))
