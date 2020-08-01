{-# LANGUAGE TemplateHaskell #-}

module Playtime.SaveLoad where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed (makeRelativeToProject, strToExp)
import My.IO
import My.Prelude
import Playtime.EngineState
import Playtime.Util

saveLocation :: FilePath
saveLocation = $(makeRelativeToProject "savegame.json" >>= strToExp)

-- FIXME: this hard codes against OneTimeEffects in EngineState. We probably don't want to hard code loading and saving
saveMay :: ToJSON gs => EngineState -> gs -> IO ()
saveMay es gs = do
  let EngineState {esActions} = es
  when (OneTimeEffect Save `setMember` esActions) $ writeFile saveLocation $ BSL.toStrict $ encode gs

loadMay :: FromJSON gs => EngineState -> IO (Maybe gs)
loadMay es = do
  let EngineState {esActions} = es
  if OneTimeEffect Load `setMember` esActions
    then do
      either fail pure . eitherDecode . BSL.fromStrict =<< readFile saveLocation
    else pure Nothing
