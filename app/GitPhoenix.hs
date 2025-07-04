module Main where

import Control.Concurrent qualified as C
import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.Extraction
import Data.Git.Phoenix.Uber
import Data.Tagged (untag)
import Relude
import UnliftIO.Directory (createDirectory)
import UnliftIO.QSem (newQSem)

main :: IO ()
main = execWithArgs go
  where
    go BuildUberRepo { inDir, outDir } = do
      createDirectory $ untag outDir
      s <- newQSem =<< C.getNumCapabilities
      runReaderT (recoverFrom inDir) (PhoenixUberConf outDir s)
    go ExtractCommitTreeAsGitRepo { rootCommit, uberRepoDir, gitRepoOut } = do
      s <- newQSem =<< C.getNumCapabilities
      runReaderT
        (extractCommitChainAsRepo rootCommit)
        (PhoenixExtractConf gitRepoOut uberRepoDir s)
    go s@SearchCommitBy {} {- author, daysBefore, uberRepoDir, daysAfter -} = -- do
      fail $ "Search is not implemented " <> show s
