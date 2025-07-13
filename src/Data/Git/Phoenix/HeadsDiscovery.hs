-- | Find commits without descendants
module Data.Git.Phoenix.HeadsDiscovery where

import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Map.Strict qualified as M
import Data.Git.Phoenix.App as A
import Data.Git.Phoenix.Commit
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Pretty
import Data.Git.Phoenix.Sha
import Data.Set qualified as S
import Data.Git.Phoenix.ShaCollision
import Data.Time.Format
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA
import Lazy.Scope qualified as S

data CommitObject
  = CommitObject
    { message :: LByteString
    , commitTs :: Int64
    , comAuthor :: LByteString
    , parent :: Maybe LByteString
    } deriving (Eq, Show, Generic)

instance NFData CommitObject

type ShaBs = LByteString

readCommitObject :: forall m . PhoenixSearchM m => FilePath -> m [(ShaBs, CommitObject)]
readCommitObject gop =
  case cutGitPath gop of
    Nothing -> pure []
    Just gp -> fmap (gp,) <$> go gop
  where
    orphanCommit parent bs =
      extractAuthor bs >>= \case
        ("", _) -> pure []
        (comAuthorBs, bs') ->
          extractCommitTs bs' >>= \case
            Nothing -> pure []
            Just (commitTs, bs'') ->
              extractMessage bs'' >>= S.toLbs >>=
              \message -> do
                comAuthor <- S.toLbs comAuthorBs
                pure [CommitObject {message, commitTs, comAuthor, parent}]

    goCommit bs =
      extractParent bs >>= \case
        ("", bs') -> orphanCommit Nothing bs'
        (parent, bs') -> (`orphanCommit` bs') =<< (Just . hexToBin <$> S.toLbs parent)

    go :: FilePath -> m [CommitObject]
    go absGop = do
      lr <- S.collapse $ do
        withCompressedH absGop $ \cbs bs ->
          classifyGitObject bs >>= \case
            Just BlobType -> pure $ Right []
            Just TreeType -> pure $ Right []
            Just CommitType -> Right <$> goCommit bs
            Just CollidedHash -> Left <$> sequenceA (fmap S.toLbs cbs)
            Nothing -> pure $ Right []
      case lr of
        Right cmt -> pure cmt
        Left cbs -> do
          disLinks <- disambiguateByPair CommitType . fmap C8.unpack $ parseFileLinks cbs
          concat <$> mapM go disLinks

loadCommitMap :: PhoenixSearchM m => m (M.Map ShaBs CommitObject)
loadCommitMap = do
  (Tagged udr) <- asks A.uberRepoDir
  runConduitRes
    (  sourceDirectoryDeep False udr
    .| mapMC readCommitObject
    .| concatC
    .| foldMC (\m (k, c) -> pure $ M.insert k c m) mempty
    )

doesMatch :: Regex -> LByteString -> Bool
doesMatch rxPat bs =
  case execute rxPat bs of
    Right (Just _) -> True
    Right _ -> False
    Left _ -> False

discoverHeads :: PhoenixSearchM m => String -> m [(ShaBs, CommitObject)]
discoverHeads authorPat = do
  case compile defaultCompOpt (ExecOption False) $ C8.pack authorPat of
    Left e -> fail $ "Invalid author regex pattern due: " <>  e
    Right authorRePat -> do
      cm <- loadCommitMap
      let parentSet = S.fromList . catMaybes . fmap parent  $ M.elems cm
          unreachable m k (v :: CommitObject) =
            if (k `S.member` parentSet) || (not $ doesMatch authorRePat (comAuthor v))
            then m
            else M.insert k v m
          ucm = M.foldlWithKey unreachable mempty cm
      pure . sortOn (\(_, c) -> (commitTs c, comAuthor c)) $ M.toList ucm

runHeadsDiscovery :: HeadsDiscovery -> IO [(ShaBs, CommitObject)]
runHeadsDiscovery HeadsDiscovery2 { author, uberRepoDir } = do
  s <- newQSem =<< liftIO getNumCapabilities
  runReaderT
    (discoverHeads author)
    (PhoenixSearchConf uberRepoDir s)

formatCommit :: (ShaBs, CommitObject) -> Doc
formatCommit (sha, co) = hsep
  [ take 8 (binSha2Str sha)
  , formatTime
      defaultTimeLocale
      "%Y-%m-%d %H:%M"
      (secondsToUtcTime . fromIntegral $ commitTs co)
  , C8.unpack (comAuthor co)
  , take 60 (C8.unpack (message co))
  ]

commitObjectsToDoc :: [(ShaBs, CommitObject)] -> Doc
commitObjectsToDoc = \case
  [] -> mempty
  o -> vcat (fmap formatCommit o) <> linebreak
