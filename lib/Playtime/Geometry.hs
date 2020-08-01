{-# OPTIONS_GHC -fno-warn-orphans #-}

module Playtime.Geometry where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zip)
import GHC.Float
import GHC.Num
import GHC.Real
import My.Prelude

type Pos = (Double, Double)

type Dim = (Double, Double)

type Scale = (Double, Double)

type Area = (Dim, Pos)

instance (Num a, Num b) => Num (a, b) where
  (lx, ly) + (rx, ry) = (lx + rx, ly + ry)
  (lx, ly) - (rx, ry) = (lx - rx, ly - ry)
  (lx, ly) * (rx, ry) = (lx * rx, ly * ry)
  negate (x, y) = (- x, - y)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)

instance (Fractional a, Fractional b) => Fractional (a, b) where
  (a, b) / (a', b') = (a / a', b / b')
  recip (a, b) = (recip a, recip b)
  fromRational r = (fromRational r, fromRational r)

isWithin :: Pos -> Area -> Bool
isWithin (cx, cy) ((width, height), (x, y)) = x <= cx && y <= cy && cx <= (x + width) && cy <= (y + height)

collidesWith :: Area -> Area -> Bool
collidesWith (da, a1) (db, b1) =
  let a2 = a1 + da; b2 = b1 + db
   in fst a1 < fst b2 && fst a2 > fst b1 && snd a1 < snd b2 && snd a2 > snd b1

cornerScales :: Corners Scale
cornerScales = Corners (0, 0) (0, 1) (1, 1) (1, 0)

data Corners a = Corners {nw :: a, sw :: a, se :: a, ne :: a} deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON, Functor) -- ne aka north east = upper left corner, etc

instance Foldable Corners where foldr f b (Corners ne se sw nw) = foldr f b [ne, se, sw, nw]

corners :: Area -> Corners Pos
corners (dim, pos) = cornerScales <&> \scale -> pos + scale * dim

move :: Double -> Area -> Pos -> Double -> Double -> [Area] -> Pos
move timePassed (objectDim, objectPos) previousPos velocityX velocityY obstacles =
  case lastMay unobstructed of
    Nothing -> objectPos
    Just pos ->
      let step (cx'', cy'') (cx', cy') =
            let goY = mfilter nonColliding $ Just $ (cx'', cy')
                goX = mfilter nonColliding $ Just $ (cx', cy'')
                xOverY = fst objectPos == fst previousPos
             in if xOverY then goX <|> goY else goY <|> goX
       in case foldM step pos $ drop (length unobstructed) candidates of
            Nothing -> pos
            Just pos' -> pos'
  where
    nonColliding p = not $ any ((objectDim, p) `collidesWith`) obstacles
    candidates = trajectoryPixels objectPos timePassed (velocityX, velocityY)
    unobstructed = takeWhile nonColliding candidates

-- given a position, a timedifference and x,y velocities - calculate relevant pixels along the trajector for checking collisions
trajectoryPixels :: Pos -> Double -> Dim -> [Pos]
trajectoryPixels objectPos timePassed ((dupe timePassed *) -> (velocityX, velocityY)) =
  -- FIXME: We should return a list of all the intersection points with pixel borders along the trajectory.
  --        What we currently do instead is wrong, but close enough for the moment.
  nub $ candidatesXY mx velocityX stepX `zip` candidatesXY my velocityY stepY
  where
    (mx, my) = objectPos
    steps :: Int
    steps = ceiling $ max (abs velocityX) (abs velocityY)
    stepX = velocityX / int2Double steps
    stepY = velocityY / int2Double steps
    candidatesXY :: Double -> Double -> Double -> [Double]
    candidatesXY base velocity step =
      (<> [base + velocity]) . toList $
        int2Double
          . (if step < 0 then floor else ceiling)
          <$> iterateN steps (+ step) base
