module Main where

import My.IO
import My.Prelude
import qualified ShootEmUp.Main

main :: IO ()
main = do
  putStrLn "running tests"
  tests
  putStrLn "starting main"
  ShootEmUp.Main.main

tests :: IO ()
tests = pure ()
