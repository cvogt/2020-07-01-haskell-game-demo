module Main where

import My.IO
import My.Prelude
import qualified SpaceMiner.Main

main :: IO ()
main = do
  putStrLn "running tests"
  tests
  putStrLn "starting main"
  SpaceMiner.Main.main

tests :: IO ()
tests = pure ()
