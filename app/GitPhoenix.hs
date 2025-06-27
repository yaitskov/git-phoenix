module Main where

import Control.Concurrent qualified as C
import Data.Git.Phoenix
import Data.Git.Phoenix.CmdArgs
import Data.Tagged (untag)
import Relude
import UnliftIO.Directory (createDirectory)
import UnliftIO.QSem (newQSem)

main :: IO ()
main = execWithArgs go
  where
    go CmdArgs { inDir, maxOpenFiles, outDir } = do
      createDirectory $ untag outDir
      nc <- C.getNumCapabilities
      when (nc < maxOpenFiles) $ C.setNumCapabilities maxOpenFiles
      s <- newQSem maxOpenFiles
      runReaderT (recoverFrom inDir) (PhoenixConf outDir s)
