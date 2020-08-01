import GHC.Err (error)
import My.IO
import My.Prelude
import Playtime.Geometry

main :: IO ()
main = do
  -- COLLISION DETECTION TESTS
  let area = (12, 1)
  assert ("corners: " <> (show $ corners area)) $
    corners area == Corners 1 (1, 13) 13 (13, 1)

  assert "1" $
    1 `isWithin` area
  assert "13" $
    13 `isWithin` area
  assert "1 13" $
    (1, 13) `isWithin` area
  assert "13 1" $
    (13, 1) `isWithin` area
  assert "14"
    $ not
    $ 14 `isWithin` area
  assert "0"
    $ not
    $ 0 `isWithin` area
  assert "1 14"
    $ not
    $ (1, 14) `isWithin` area
  assert "1 0"
    $ not
    $ (1, 0) `isWithin` area
  assert "0 13"
    $ not
    $ (0, 13) `isWithin` area
  assert "13 12"
    $ not
    $ (14, 13) `isWithin` area
  assert "0 6"
    $ not
    $ (0, 6) `isWithin` area
  assert "6 0"
    $ not
    $ (6, 0) `isWithin` area

  assert "144 96 within"
    $ not
    $ (156.1, 107.01540000000021) `isWithin` (12, (144, 96))

  assert "144 96"
    $ not
    $ (12, (156.1, 107.01540000000021)) `collidesWith` (12, (144, 96))

  assert "144 108"
    $ not
    $ (12, (156.1, 107.01540000000021)) `collidesWith` (12, (144, 108))

assert :: [Char] -> Bool -> IO ()
assert msg predicate =
  if predicate
    then putStrLn ("SUCCESS: " <> msg)
    else error ("FAILED: " <> msg)
