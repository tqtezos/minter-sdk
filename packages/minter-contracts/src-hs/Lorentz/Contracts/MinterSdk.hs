-- | Commons for this repository.
module Lorentz.Contracts.MinterSdk
  ( inBinFolder
  ) where

import System.Environment (lookupEnv)
import System.FilePath ((</>))

getBinFolder :: IO FilePath
getBinFolder = do
  mpath <- lookupEnv "TZ_BIN_PATH"
  return (mpath ?: "bin")

inBinFolder :: FilePath -> IO FilePath
inBinFolder file = do
  binFolder <- getBinFolder
  return (binFolder </> file)
