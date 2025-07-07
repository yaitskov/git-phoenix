module Data.Git.Phoenix.CommitSearch where

import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs (DaysAfter, DaysBefore)
import Data.Git.Phoenix.Commit
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Pretty
import Data.Git.Phoenix.Sha
import Data.Git.Phoenix.ShaCollision
import Data.Time.Clock.System
import Data.Time.Format
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA

data CommitObject
  = CommitObject
    { message :: LByteString
    , sha :: LByteString
    , commitTs :: Int64
    , author :: LByteString
    } deriving (Eq, Show, Generic)

instance NFData CommitObject

readCommitObject :: forall m . PhoenixSearchM m => GitPath Commit -> m [CommitObject]
readCommitObject gop = go . (</> toFp gop) . untag =<< asks uberRepoDir
  where
    -- "commit 192\NULtree 844eaa6a04859d069e9ae10f2c6c293d23efc459\nauthor Daniil Iaitskov <dyaitskov@gmail.com> 1750985584 -0800\ncommitter Daniil Iaitskov <dyaitskov@gmail.com> 1 750 985 584 -0800\n\n init git-phoenix\n"
    goCommit bs =
      case extractAuthor bs of
        ("", _) -> pure []
        (author, bs') ->
          case extractCommitTs bs' of
            Nothing -> pure []
            Just (commitTs, bs'') ->
              case extractMessage bs'' of
                message ->
                  let sha = gitPath2Bs gop in
                    pure [CommitObject {message, sha, commitTs, author}]

    go :: FilePath -> m [CommitObject]
    go absGop = do
      lr <- withCompressedH absGop $ \cbs bs ->
        case classifyGitObject bs of
          Just BlobType -> pure $ Right []
          Just TreeType -> pure $ Right []
          Just CommitType -> Right <$> goCommit bs
          Just CollidedHash -> pure $ Left cbs
          Nothing -> pure $ Right []
      case lr of
        Right cmt -> pure cmt
        Left cbs -> do
          disLinks <- disambiguateByPair CommitType . fmap C8.unpack $ parseFileLinks cbs
          concat <$> mapM go disLinks

parseGitObject :: PhoenixSearchM m =>
  Regex ->
  Tagged DaysAfter Int64 ->
  Tagged DaysBefore Int64 ->
  FilePath ->
  m [CommitObject]
parseGitObject authorRegex (Tagged epochSecondsAfter) (Tagged epochSecondsBefore) fp =
  filter commitPredicate <$> readCommitObject (GitPath fp)
  where
    commitPredicate co =
      case execute authorRegex (author co) of
        Right _ ->
          commitTs co >= epochSecondsAfter && commitTs co <= epochSecondsBefore
        Left _ -> False

searchCommit :: PhoenixSearchM m =>
  String -> Tagged DaysAfter Int -> Tagged DaysBefore Int -> m ()
searchCommit authorPat daysAfter daysBefore = do
  (Tagged udr) <- asks uberRepoDir
  now <- liftIO (systemSeconds <$> getSystemTime)
  let daysToEpochSecs :: forall x. Tagged x Int -> Tagged x Int64
      daysToEpochSecs = fmap ((now -) . (* 86400)) . fromIntegral
  case compile defaultCompOpt (ExecOption False) $ C8.pack authorPat of
    Left e -> fail $ "Invalid author regex pattern due: " <>  e
    Right authorRePat -> do
      matchingCommits <- sortOn commitTs <$> runConduitRes
        (  sourceDirectoryDeep False udr
        .| mapMC (parseGitObject authorRePat
                   (daysToEpochSecs daysAfter)
                   (daysToEpochSecs daysBefore))
        .| foldMC (\l a -> pure $ a <> l) []
        )
      case matchingCommits of
        [] -> pure ()
        o -> printDoc $ vcat (fmap formatCommit o) <> linebreak

formatCommit :: CommitObject -> Doc
formatCommit co = hsep
  [ take 8 (binSha2Str (sha co))
  , formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (secondsToUtcTime . fromIntegral $ commitTs co)
  , C8.unpack (author co)
  , take 60 (C8.unpack (message co))
  ]
