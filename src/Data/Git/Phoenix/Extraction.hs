module Data.Git.Phoenix.Extraction where

import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.Commit
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Sha
import Data.Tagged (Tagged (..), untag)
import Data.Git.Phoenix.Repo
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Tree
import Relude
import System.FilePath ((</>))

readCommitObject :: PhoenixExtractM m => GitPath Commit -> m (Maybe (GitPath Commit), GitPath Tree)
readCommitObject gop = (`withCompressed` go) . (</> toFp gop) . untag =<< asks uberDir
  where
    go bs = case classifyGitObject bs of
      Just BlobType -> fail $ show gop <> " is Git blob but expected Git commit"
      Just TreeType -> fail $ show gop <> " is Git tree but expected Git commit"
      Just CommitType ->
        case extractTreeHash bs of
          ("", _) -> fail $ show gop <> " does not have tree field"
          (treeComit, bs') ->
            case extractParent bs' of
              ("", _) -> pure (Nothing, shaToPath $ L8.unpack treeComit)
              (ph, _) -> pure ( Just . shaToPath $ L8.unpack ph
                              , shaToPath $ L8.unpack treeComit
                              )
      Just CollidedHash ->
        -- read list of colliding files
        -- check previous choice
        -- ask user choice (specific number, random, RANDOM - for all)
        -- remember choice if not random
        -- do readCommitObject recursively
        fail $ show gop <> " has SHA collision"
      Nothing -> fail $ show gop <> " is not a Git commit object"

extractCommit :: PhoenixExtractM m => GitPath Commit ->  m ()
extractCommit ohp = do
  (mParHash, treeHash) <- readCommitObject ohp
  extractTree treeHash
  mapM_ extractCommit mParHash

extractCommitChainAsRepo :: PhoenixExtractM m => Tagged ShaPrefix String -> m ()
extractCommitChainAsRepo (Tagged rootCommit) = do
  gitDir <- untag <$> asks destGitDir
  initGitRepo gitDir
  extractCommit (shaToPath rootCommit)

    -- (Tagged $ gitDir </> ".git" </> "objects")
