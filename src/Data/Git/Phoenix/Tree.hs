module Data.Git.Phoenix.Tree where

import Data.ByteString.Lazy qualified as L
import Data.Git.Phoenix.App
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Sha
import Data.Git.Phoenix.ShaCollision
import Data.Git.Phoenix.Io

dropTreeHeader :: LByteString -> LByteString
dropTreeHeader = L.drop 1 . L.dropWhile (/= 0)

data DOF = Dir | File deriving (Eq, Show, Generic)

instance NFData DOF

dofToGitObjType :: DOF -> GitObjType
dofToGitObjType =
  \case
    Dir -> TreeType
    File -> BlobType

readTreeShas :: LByteString -> [(DOF, LByteString)]
readTreeShas modePrefixedBs =
  case L.uncons modePrefixedBs of
    Just (0x31, bs) -> go File bs {- '1' blob  -}
    Just (0x34, bs) -> go Dir bs  {- '4' tree  -}
    Nothing -> []
    Just (ue, _) ->
      error $ "tree entry mode does not start with 1 nor 4: "
        <> show ue <> "\n" <> show modePrefixedBs
  where
    shaBinLen = 20
    go dof bs =
      case L.uncons $ L.dropWhile (/= 0) bs  of
        Just (0, shaPrefixedBs) ->
          let (sha, bs') = L.splitAt shaBinLen shaPrefixedBs in
            (dof, sha) : readTreeShas bs'
        Just (nz, _) ->
          error $ "expected zero byte but got " <> show nz <> " in "
            <> show modePrefixedBs
        Nothing ->
          error $ "unexpected end of tree entry: " <> show modePrefixedBs

-- Type is defined to decouple reading files and handling there content.
-- Such trick minimize QSem
data NonRecursive
  = JustBlob !()
  | TreeShas ![(DOF, LByteString)]
  -- collision strict BS is not big just list of file names
  -- so it is safe to return out of lazy scope
  | Collision !(Tagged Compressed LByteString)
  deriving (Show, Eq, Generic)

instance NFData NonRecursive

parseTreeObject :: PhoenixExtractM m =>
  FilePath ->
  Tagged Compressed LByteString ->
  LByteString ->
  m (Either (Tagged Compressed LByteString) [(DOF, LByteString)])
parseTreeObject gop cbs bs =
  case classifyGitObject bs of
    Just BlobType -> fail $ gop <> " is Git blob but expected Git tree"
    Just CommitType -> fail $ gop <> " is Git commit but expected Git tree"
    Just TreeType -> do
      pure . Right . readTreeShas $ dropTreeHeader bs
    Just CollidedHash -> pure $ Left cbs
    Nothing -> fail $ gop <> " is not a Git tree object"

onRight_ :: Monad m => (b -> m ()) -> Either a b -> m (Either a b)
onRight_ f = \case
  v@(Left _) -> pure v
  r@(Right v) -> f v >> pure r

extractTree :: PhoenixExtractM m => GitPath Tree -> m ()
extractTree treeHash = do
  Tagged udr <- asks uberDir
  dd <- getDestDir
  -- putStrLn $ " udr </> toFp treeHash: " <> (udr </> toFp treeHash) <> " ; dd = " <> dd
  copyTree (udr </> toFp treeHash) treeHash >>=
    mapM_ (copyTreeLinks dd) . $(tw "len/")
  where
    copyTree treePath trH = do
      let save bs = do
            destDir <- getDestDir
            saveCompressedBs (destDir </> toFp trH) bs
      !rl <- withCompressedH treePath $ \cTreeBs treeBs ->
        parseTreeObject treePath cTreeBs treeBs >>= onRight_ (\_ -> save treeBs)
      shas <- case rl of
        Right shas' -> pure shas'
        Left cbs -> do
          uniPath <- uniqBs (GitPath @Tree treePath) cbs TreeType
          withCompressed uniPath
            (\ubs -> do
                save ubs
                pure . readTreeShas $! dropTreeHeader ubs
            )
      pure shas
    getDestDir = (\(Tagged r) -> r </> ".git" </> "objects") <$> asks destGitDir
    copyTreeLinks destDir (dof, binSha) = do
      (Tagged udr) <- asks uberDir
      liftIO $(trIo "/destDir binSha")
      let shaP = binSha2Path binSha
          absSha = udr </> toFp shaP
          saveBlob = saveCompressedBs (destDir </> toFp shaP)
          saveTree bs = do
            saveBlob bs
            pure . readTreeShas $ dropTreeHeader bs
      nonRec <- withCompressedH absSha $ \cbs bs ->
        case classifyGitObject bs of
          Just BlobType
            | dof == File -> JustBlob <$!> saveBlob bs
            | otherwise -> fail $ absSha <> " is not a GIT blob"
          Just TreeType
            | dof == Dir -> TreeShas <$!> saveTree bs
            | otherwise -> fail $ absSha <> " is not a GIT tree"
          Just CollidedHash ->
            pure $! Collision cbs
          _ -> fail $ absSha <> " is not a GIT tree nor GIT blob nor disambiguate file"
      case nonRec of
        JustBlob () -> pure ()
        TreeShas rows ->
          mapM_ (copyTreeLinks destDir) rows
        Collision cbs' -> do
          uniPath <- uniqBs shaP cbs' (dofToGitObjType dof)
          !lr <- withCompressed uniPath $ \ubs ->
            case classifyGitObject ubs of
              Just BlobType -> Left <$!> saveBlob ubs
              Just TreeType -> Right <$!> saveTree ubs
              _ -> fail $ absSha <> " is not GIT tree nor GIT blob"
          case lr of
            Left () -> pure ()
            Right rows -> mapM_ (copyTreeLinks destDir) rows
