
module Main where

import Data.Aeson
import System.Environment
import qualified Data.ByteString.Lazy as B

import Types

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

      
