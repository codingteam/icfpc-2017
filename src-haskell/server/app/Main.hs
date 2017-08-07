module Main where

import Data.Aeson
import System.Environment
import qualified Data.ByteString.Lazy as B

import Types
import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--test-map", mapPath] -> do
      content <- B.readFile mapPath
      let Just map = decode content :: Maybe GameMap
      print map
    ["--test-parse", path] -> do
      content <- B.readFile path
      let Just msg = decode content :: Maybe ServerMessage
      print msg
    ["--host-a-game", port, mapPath, numberOfPlayers_] -> do
      mapContent <- B.readFile mapPath
      let Just map = decode mapContent :: Maybe GameMap

      let numberOfPlayers = read numberOfPlayers_ :: Int

      runServer map port numberOfPlayers
