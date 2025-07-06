module Data.Git.Phoenix.CmdRun where

import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.Extraction
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Uber

runCmd :: CmdArgs -> IO ()
runCmd = \case
  BuildUberRepo { inDir, outDir } -> do
    createDirectory $ untag outDir
    s <- newQSem =<< getNumCapabilities
    runReaderT (recoverFrom inDir) (PhoenixUberConf outDir s)
  ExtractCommitTreeAsGitRepo { rootCommit, uberRepoDir, gitRepoOut } -> do
    s <- newQSem . $(tw "numCapabilities/") =<< getNumCapabilities

    runReaderT
      (extractCommitChainAsRepo rootCommit)
      (PhoenixExtractConf gitRepoOut uberRepoDir s)
  s@SearchCommitBy {} {- author, daysBefore, uberRepoDir, daysAfter -} -> -- do
    fail $ "Search is not implemented " <> show s
