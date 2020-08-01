module Playtime.Util where

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import GHC.Float (int2Double)
import My.Prelude
import Playtime.Event
import Playtime.Geometry

timeDiffPico :: SystemTime -> SystemTime -> Integer
timeDiffPico before after = diffTimeToPicoseconds $ diffAbsoluteTime (systemToTAITime after) (systemToTAITime before)

pico2second :: Double -> Double
pico2second picosecs = picosecs / 1000 / 1000 / 1000 / 1000

pico2Double :: Integral i => i -> Double
pico2Double pico = int2Double (fromIntegral pico) / 1000 / 1000 / 1000 / 1000

avg :: Foldable t => t Integer -> Double
avg xs = (fromInteger @Double $ sum xs) / (int2Double $ length xs)

allEnumValues :: forall a. (Enum a, Bounded a) => [a]
allEnumValues = enumFrom (minBound :: a)

mod2 :: Pos -> Pos -> Pos
mod2 = pairWise mod' mod'

pairWise :: (a -> b -> c) -> (a' -> b' -> c') -> (a, a') -> (b, b') -> (c, c')
pairWise f g (a, a') (b, b') = (f a b, g a' b')

---

--- stuff below needs cleanup or removal:
data OneTimeEffect' = Load | Save | Reset deriving (Eq, Ord, Show, Generic, NFData)

data MovementAction' = Up | Down | Left' | Right' deriving (Eq, Ord, Show, Generic, NFData)

data Action = OneTimeEffect OneTimeEffect' | Exit | MovementAction MovementAction' deriving (Eq, Ord, Show, Generic, NFData)

oneTimeEffectMay :: Action -> Maybe OneTimeEffect'
oneTimeEffectMay (OneTimeEffect v) = Just v
oneTimeEffectMay _ = Nothing

movementAction :: Action -> Maybe MovementAction'
movementAction (MovementAction v) = Just v
movementAction _ = Nothing

groupKeyBindings :: [([Key], Action)] -> Map Key [(Set Key, Action)]
groupKeyBindings keyBindingsRaw = mapFromList $ groups <&> \l@(h :| _) -> (fst h, first setFromList <$> (join . toList $ snd <$> l))
  where
    groups :: [NonEmpty (Key, [([Key], Action)])]
    groups = groupAllWith fst $ join $ keyBindingsRaw <&> (\b@(keys', _) -> (,[b]) <$> keys')

keyBindings :: [([Key], Action)]
keyBindings =
  [ ([Key'LeftSuper, Key'Q], Exit),
    ([Key'Escape], Exit),
    ([Key'LeftSuper, Key'L], OneTimeEffect Load),
    ([Key'LeftSuper, Key'S], OneTimeEffect Save),
    ([Key'LeftSuper, Key'R], OneTimeEffect Reset),
    ([Key'W], MovementAction Up),
    ([Key'S], MovementAction Down),
    ([Key'A], MovementAction Left'),
    ([Key'D], MovementAction Right')
  ]
