module Main where

import Data.Git.Phoenix
import Data.Git.Phoenix.CmdArgs
import Data.Tagged (untag)
import Relude
import System.Directory (createDirectory)

main :: IO ()
main = execWithArgs go
  where
    go CmdArgs { inDir, outDir } = do
      createDirectory $ untag outDir
      runReaderT (recoverFrom inDir) (PhoenixConf outDir)
