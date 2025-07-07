module Data.Git.Phoenix.Extraction where

import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.Commit
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Repo
import Data.Git.Phoenix.Sha
import Data.Git.Phoenix.ShaCollision
import Data.Git.Phoenix.Tree


readCommitObject :: PhoenixExtractM m => GitPath Commit -> m (Maybe (GitPath Commit), GitPath Tree)
readCommitObject gop = go . (</> toFp gop) . untag =<< asks uberDir
  where
    goCommit bs =
      case extractTreeHash $ $(tr "eee/bs") bs of
        ("", _) -> fail $ show gop <> " does not have tree field"
        (treeComit, bs') -> do
          gitDir <- untag <$> asks destGitDir
          saveCompressedBs
            (gitDir </> ".git" </> "objects" </> toFp gop)
            bs
          case extractParent bs' of
            ("", _) -> pure (Nothing, shaToPath $ L8.unpack treeComit)
            (!ph, _) -> pure ( Just . shaToPath $ L8.unpack ph
                            , $(tr "/treeComit") . shaToPath $ L8.unpack treeComit
                            )
    go absGop = do
      lr <- withCompressedH absGop $ \cbs bs ->
        case classifyGitObject bs of
          Just BlobType -> fail $ show gop <> " is Git blob but expected Git commit"
          Just TreeType -> fail $ show gop <> " is Git tree but expected Git commit"
          Just CommitType -> Right <$> goCommit bs
          Just CollidedHash -> pure $ Left cbs
          Nothing -> fail $ show gop <> " is not a Git commit object"
      case lr of
        Right cmt -> pure cmt
        Left cbs -> do
            uniPath <- uniqBs gop cbs CommitType
            withCompressed uniPath $ \ubs ->
              case classifyGitObject ubs of
                Just CommitType -> goCommit ubs
                ops -> fail $ "Uniq BS of " <> show gop <> " is not commit but " <> show ops


extractCommit :: PhoenixExtractM m => GitPath Commit -> m ()
extractCommit ohp = do
  liftIO $(trIo "/ohp")
  (mParHash, treeHash) <- readCommitObject ohp
  extractTree $ $(tw "/") treeHash
  mapM_ extractCommit mParHash

extractCommitChainAsRepo :: PhoenixExtractM m => Tagged ShaPrefix String -> m ()
extractCommitChainAsRepo (Tagged rootCommit) = do
  (Tagged udr) <- asks uberDir
  completePath (udr </> (toFp $ shaToPath rootCommit)) >>= \case
    [up] -> do
      gitDir <- untag <$> asks destGitDir
      initGitRepo gitDir
      let uc = GitPath . $(tw "/udr up") $ makeRelative udr up
      extractCommit uc
      withBinaryFile
        (gitDir </> ".git" </> "refs" </> "heads" </> "master")
        WriteMode
        (`hPut` toCommitSha uc)
    [] -> fail $ "No commit matching prefix: " <> show rootCommit
    ambiP -> fail $ "Commit prefix is ambioguous:\n " <> intercalate "\n" ambiP

completePath :: MonadUnliftIO m => FilePath -> m [FilePath]
completePath fp = do
  ifM (doesFileExist fp) (pure [fp]) $ do
    ifM (doesDirectoryExist fp)
      (completeNonEmptyDir fp id) $ do
        case splitFileName fp of
          (dp, fpre) ->
            completeNonEmptyDir dp (filter (fpre `isPrefixOf`))
  where
    completeNonEmptyDir dp fnf =
      listDirectory dp >>= (\case [] -> pure [dp] ; o -> pure $ fmap (dp </>) o) . fnf
