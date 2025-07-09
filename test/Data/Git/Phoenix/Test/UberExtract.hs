module Data.Git.Phoenix.Test.UberExtract where

import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.CmdRun
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Test
import UnliftIO.Process

unit_uber_extract :: IO ()
unit_uber_extract = withUber go
  where
    go :: Tagged Root FilePath -> Tagged Uber FilePath -> IO ()
    go (Tagged rdir) (Tagged uberOut) = do
      let gitOut = rdir </> "git-phoenix"
      runCmd ExtractCommitTreeAsGitRepo { rootCommit = Tagged $ take 15 currentHead
                                        , uberRepoDir = Tagged uberOut
                                        , gitRepoOut = Tagged gitOut
                                        }
      callCommand $ "git -C " <> gitOut <> " fsck --full"
      gotHead <- readBranchCommit (gitOut </> ".git/refs/heads/master")
      when (gotHead /= currentHead) $ do
        fail $ "HEAD commit mismatch\nGot:      [" <> gotHead
                               <> "]\nExpected: [" <> currentHead <> "]"
