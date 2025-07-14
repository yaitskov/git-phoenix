module Data.Git.Phoenix.CommitSearch where

import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Git.Phoenix.App as A
import Data.Git.Phoenix.CmdArgs (DaysAfter, DaysBefore, SearchCommitBy (..))
import Data.Git.Phoenix.Commit
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Pretty
import Data.Git.Phoenix.Sha
import Data.Git.Phoenix.ShaCollision
import Data.List.NonEmpty (groupWith)
import Data.Time.Clock.System
import Data.Time.Format
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA
import Lazy.Scope ( toLbs, unScope, bs2Scoped )

data CommitObject
  = CommitObject
    { message :: LByteString
    , sha :: LByteString
    , commitTs :: Int64
    , author :: LByteString
    } deriving (Eq, Show, Generic)

instance NFData CommitObject

readCommitObject :: forall m . PhoenixSearchM m => FilePath -> m [CommitObject]
readCommitObject = go
  where
    -- "commit 192\NULtree 844eaa6a04859d069e9ae10f2c6c293d23efc459\nauthor Daniil Iaitskov <dyaitskov@gmail.com> 1750985584 -0800\ncommitter Daniil Iaitskov <dyaitskov@gmail.com> 1 750 985 584 -0800\n\n init git-phoenix\n"
    goCommit bs =
      extractAuthor bs >>= \case
        ("", _) -> pure []
        (authorBs, bs') ->
          extractCommitTs bs' >>= \case
            Nothing -> pure []
            Just (commitTs, bs'') ->
              extractMessage bs'' >>= toLbs >>=
                \message -> do
                  author <- toLbs authorBs
                  sha <- unScope $ bs2Scoped (gitPath2Bs . shaToPath . showDigest . sha1) bs
                  pure [CommitObject {message, sha, commitTs, author}]


    go :: FilePath -> m [CommitObject]
    go absGop = do
      lr <-
        withCompressedH absGop $ \cbs bs ->
          classifyGitObject bs >>= \case
            Just BlobType -> pure $ Right []
            Just TreeType -> pure $ Right []
            Just CommitType -> Right <$> goCommit bs
            Just CollidedHash -> Left <$> sequenceA (fmap toLbs cbs)
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
  filter commitPredicate <$> readCommitObject fp
  where
    commitPredicate co =
      case execute authorRegex (Data.Git.Phoenix.CommitSearch.author co) of
        Right (Just _) ->
          commitTs co >= epochSecondsAfter && commitTs co <= epochSecondsBefore
        Right _ -> False
        Left _ -> False

dedupOrderedList :: Eq a => [a] -> [a]
dedupOrderedList = fmap head . groupWith id

searchCommit :: PhoenixSearchM m =>
  String -> Tagged DaysAfter Int -> Tagged DaysBefore Int -> m [CommitObject]
searchCommit authorPat daysAfter daysBefore = do
  (Tagged udr) <- asks A.uberRepoDir
  now <- liftIO (systemSeconds <$> getSystemTime)
  let daysToEpochSecs :: forall x. Tagged x Int -> Tagged x Int64
      daysToEpochSecs = fmap ((now -) . (* 86400)) . fromIntegral
  case compile defaultCompOpt (ExecOption False) $ C8.pack authorPat of
    Left e -> fail $ "Invalid author regex pattern due: " <>  e
    Right authorRePat ->
      dedupOrderedList . sortOn commitTs <$> runConduitRes
        (  sourceDirectoryDeep False udr
        .| mapMC (parseGitObject authorRePat
                   (daysToEpochSecs daysAfter)
                   (daysToEpochSecs daysBefore))
        .| foldMC (\l a -> pure $ a <> l) []
        )

commitObjectsToDoc :: [CommitObject] -> Doc
commitObjectsToDoc = \case
  [] -> mempty
  o -> vcat (fmap formatCommit o) <> linebreak

runCommitSearch :: SearchCommitBy -> IO [CommitObject]
runCommitSearch SearchCommitBy2 { author, daysBefore, uberRepoDir, daysAfter } = do
  s <- newQSem =<< liftIO getNumCapabilities
  runReaderT
    (searchCommit author daysAfter daysBefore)
    (PhoenixSearchConf uberRepoDir s)

formatCommit :: CommitObject -> Doc
formatCommit co = hsep
  [ take 8 (binSha2Str (sha co))
  , formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (secondsToUtcTime . fromIntegral $ commitTs co)
  , C8.unpack (Data.Git.Phoenix.CommitSearch.author co)
  , take 60 (C8.unpack (message co))
  ]
