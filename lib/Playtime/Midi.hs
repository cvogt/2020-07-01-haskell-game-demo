{-# LANGUAGE TemplateHaskell #-}

module Playtime.Midi where

import Data.FileEmbed (makeRelativeToProject, strToExp)
import Euterpea hiding (Text, forever)
import My.IO
import My.Prelude
import System.Posix.Process (executeFile)

spawnSynth :: IO ()
spawnSynth =
  executeFile "/usr/local/bin/fluidsynth" False [$(makeRelativeToProject "Steinway+Grand+Piano+ER3A.sf2" >>= strToExp)] Nothing

playMusic :: IO ()
playMusic = do
  let p = Euterpea.play . Euterpea.line
  -- let p1 = [c 5 qn, e 5 qn, d 5 en, c 5 en, e 5 qn, c 5 qn, b 4 en, e 5 en, a 4 hn]
  -- let p2 = [e 5 qn, e 5 qn, g 5 qn, g 5 en, a 5 en, f 5 qn, a 5 qn, e 5 hn]
  -- let p3 = [c 6 qn, b 5 en, a 5 en, b 5 qn, e 5 qn, a 5 qn, e 5 en, d 5 en, e 5 qn, b 4 qn]
  -- p $ p1 <> p2 <> p3
  -- p $ [chord [c 4 en, e 4 en, g 4 en], chord [c 4 en, e 4 en, g 4 en], chord [c 4 en, e 4 en, g 4 en], chord [a 3 hn, c 4 hn, e 4 hn]] -- p1 <> p2 <> p3
  let loykrathong =
        [c 3 en, c 3 en, a 2 en, c 3 en, d 3 en, f 3 qn] <> [g 3 en, f 3 en, d 3 qn, c 3 en, d 3 en, c 3 en, a 2 qn]
          <> [c 3 en, d 3 en, f 3 en, d 3 en, f 3 qn]
          <> [c 3 en, d 3 en, f 3 en, g 3 qn, f 3 en, d 3 en, c 3 en, c 3 qn]
          <> [f 3 en, f 3 en, f 3 en, f 3 en, f 3 en, f 3 qn]
          <> [c 3 en, c 3 en, c 3 en, c 3 en, c 3 en, c 3 qn]
          <> [f 3 en, f 3 en, f 3 en, g 3 en, a 3 qn]
          <> [g 3 en, f 3 en, d 3 en, c 3 en, d 3 en, f 3 en, g 3 en, a 3 qn]
          <> [a 3 en, a 3 en, a 3 en, a 3 en, a 3 en, a 3 qn]
          <> [f 3 en, f 3 en, f 3 en, f 3 en, f 3 en, f 3 qn]
          <> [f 3 en, d 3 en, c 3 en, d 3 en, f 3 en, d 3 en, f 3 qn]
          <> [f 3 en, d 3 en, c 3 en, d 3 en, f 3 en, d 3 en, f 3 qn]
  p $ tempo 0.75 <$> loykrathong

-- https://github.com/igorski/VSTSID
-- http://hackage.haskell.org/package/csound-expression-typed-0.2.2.0/docs/src/Csound.Typed.Plugins.Diode.html
-- https://sites.google.com/site/fluidvolt/
-- https://musical-artifacts.com/artifacts/185
-- https://trisamples.com/free-soundfonts/
-- http://drunk3nj3sus.blogspot.com/2015/04/c64-waveform-soundfonts.html
