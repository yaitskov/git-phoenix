module Data.Git.Phoenix.CmdRun where

import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.CommitSearch
import Data.Git.Phoenix.Extraction
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Pretty
import Data.Git.Phoenix.Uber
import Data.Version (showVersion)
import Paths_git_phoenix

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
  SearchCommitBy scb ->
    runCommitSearch scb >>= printDoc . commitObjectsToDoc
  GitPhoenixVersion ->
    printDoc $ "Version" <+> doc (showVersion version) <> linebreak
