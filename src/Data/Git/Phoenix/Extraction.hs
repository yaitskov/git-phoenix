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

class MyNf a where
  myRnf :: a -> ()

instance MyNf a => MyNf (Tagged x a) where
  myRnf = myRnf . untag

instance MyNf LByteString where
  myRnf x = rnf $! L8.length x

data EitherS a b = LeftS !a | RightS !b deriving (Show, Eq, Generic)

instance (NFData a,  NFData b) => NFData (EitherS a b)
-- instance (NFData a,  NFData b) => NFData (EitherS a b) where
--   rnf = \case
--     LeftS x -> rnf x
--     RightS x -> rnf x

goCommit gop bs =
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

go gop absGop = do
  lr <- withCompressedH absGop $ \cbs bs ->
    case classifyGitObject bs of
      Just BlobType -> fail $ show gop <> " is Git blob but expected Git commit"
      Just TreeType -> fail $ show gop <> " is Git tree but expected Git commit"
      Just CommitType -> RightS <$> goCommit gop bs
      Just CollidedHash -> do
        -- putStrLn $ "Collided Len " <> show (fmap L8.length cbs)
        pure $ LeftS cbs
      Nothing -> fail $ show gop <> " is not a Git commit object"
  case lr of
    RightS cmt -> pure cmt
    LeftS cbs -> do
        uniPath <- uniqBs gop cbs CommitType
        withCompressed uniPath $ \ubs ->
          case classifyGitObject ubs of
            Just CommitType -> goCommit gop ubs
            ops -> fail $ "Uniq BS of " <> show gop <> " is not commit but " <> show ops

readCommitObject :: PhoenixExtractM m => GitPath Commit -> m (Maybe (GitPath Commit), GitPath Tree)
readCommitObject gop = go gop . (</> toFp gop) . untag =<< asks uberDir

extractCommit :: PhoenixExtractM m => GitPath Commit -> m ()
extractCommit ohp = do
  liftIO $(trIo "/ohp")
  (mParHash, treeHash) <- readCommitObject ohp
  extractTree $ $(tw "/") treeHash
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
