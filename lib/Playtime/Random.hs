module Playtime.Random where

import Data.List (zip)
import GHC.Float (double2Int, int2Double)
import My.Prelude
import Playtime.Geometry
import System.Random

randomsR :: (Random a) => StdGen -> Int -> (a, a) -> ([a], StdGen)
randomsR = randomsR' . ([],)
  where
    randomsR' :: (Random a) => ([a], StdGen) -> Int -> (a, a) -> ([a], StdGen)
    randomsR' res n _ | n <= 0 = res
    randomsR' (acc, g') n r = randomsR' (first (: acc) $ randomR r g') (n -1) r

randomPoss :: StdGen -> Int -> Dim -> ([Pos], StdGen)
randomPoss g n (maxX, maxY) =
  let (xs, g') = randomsNatDouble g n maxX
      (ys, g'') = randomsNatDouble g' n maxY
   in (xs `zip` ys, g'')

randomsNatDouble :: StdGen -> Int -> Double -> ([Double], StdGen)
randomsNatDouble g num maxV = first (fmap int2Double) $ randomsR g num (0, double2Int maxV)
