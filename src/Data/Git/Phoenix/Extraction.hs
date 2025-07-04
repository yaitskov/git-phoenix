module Data.Git.Phoenix.Extraction where

import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.Commit
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Sha
import Data.Git.Phoenix.ShaCollision
import Data.Git.Phoenix.Repo
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Tree

readCommitObject :: PhoenixExtractM m => GitPath Commit -> m (Maybe (GitPath Commit), GitPath Tree)
readCommitObject gop = (`withCompressedH` go) . (</> toFp gop) . untag =<< asks uberDir
  where
    goCommit bs =
      case extractTreeHash $ $(tr "!eee/bs") bs of
        ("", _) -> fail $ show gop <> " does not have tree field"
        (treeComit, bs') ->
          case extractParent bs' of
            ("", _) -> pure (Nothing, shaToPath $ L8.unpack treeComit)
            (ph, _) -> pure ( Just . shaToPath $ L8.unpack ph
                            , $(tr "/treeComit") . shaToPath $ L8.unpack treeComit
                            )

    go cbs bs = case classifyGitObject bs of
      Just BlobType -> fail $ show gop <> " is Git blob but expected Git commit"
      Just TreeType -> fail $ show gop <> " is Git tree but expected Git commit"
      Just CommitType -> goCommit bs
      Just CollidedHash -> do
        ubs <- uniqBs gop cbs CommitType
        case classifyGitObject ubs of
          Just CommitType -> goCommit ubs
          ops -> fail $ "Uniq BS of " <> show gop <> " is not commit but " <> show ops
      Nothing -> fail $ show gop <> " is not a Git commit object"

extractCommit :: PhoenixExtractM m => GitPath Commit -> m ()
extractCommit ohp = do
  (mParHash, treeHash) <- readCommitObject ohp
  extractTree $ $(tw "!/") treeHash
  mapM_ extractCommit mParHash

extractCommitChainAsRepo :: PhoenixExtractM m => Tagged ShaPrefix String -> m ()
extractCommitChainAsRepo (Tagged rootCommit) = do
  gitDir <- untag <$> asks destGitDir
  initGitRepo gitDir
  extractCommit (shaToPath rootCommit)
  withBinaryFile
    (gitDir </> ".git" </> "refs" </> "heads" </> "master")
    WriteMode
    (`hPut` L8.pack rootCommit)

    -- (Tagged $ gitDir </> ".git" </> "objects")
