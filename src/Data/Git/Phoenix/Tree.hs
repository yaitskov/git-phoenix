module Data.Git.Phoenix.Tree where

import Codec.Compression.Zlib qualified as Z
import Data.ByteString.Lazy qualified as L
import Data.Git.Phoenix.App
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Sha
import Data.Git.Phoenix.ShaCollision
import Data.Tagged (Tagged (..))
import Data.Git.Phoenix.Io
import Relude
import System.FilePath ((</>))
import UnliftIO.IO qualified as U


dropTreeHeader :: L.ByteString -> L.ByteString
dropTreeHeader = L.drop 1 . L.dropWhile (/= 0)

data DOF = Dir | File deriving (Eq, Show, Generic)

instance NFData DOF

dofToGitObjType :: DOF -> GitObjType
dofToGitObjType =
  \case
    Dir -> TreeType
    File -> BlobType

readTreeShas :: L.ByteString -> [(DOF, L.ByteString)]
readTreeShas modePrefixedBs =
  case L.uncons modePrefixedBs of
    Just (0x31, bs) -> go File bs {- '1' blob  -}
    Just (0x34, bs) -> go Dir bs  {- '4' tree  -}
    Nothing -> []
    _ ->
      error $ "tree entry mode does not start with 1 nor 4: "
        <> show modePrefixedBs
  where
    shaBinLen = 20
    go dof bs =
      case L.uncons $ L.dropWhile (/= 0) bs  of
        Just (0, shaPrefixedBs) ->
          let (sha, bs') = L.splitAt shaBinLen shaPrefixedBs in
            (dof, sha) : (readTreeShas $ L.dropWhile (/= 0) bs')
        Just (nz, _) ->
          error $ "expected zero byte but got " <> show nz <> " in "
            <> show modePrefixedBs
        Nothing ->
          error $ "unexpected end of tree entry: " <> show modePrefixedBs

parseTreeObject :: PhoenixExtractM m =>
  GitPath Tree ->
  Tagged Compressed L.ByteString ->
  L.ByteString ->
  m [(DOF, L.ByteString)]
parseTreeObject gop cbs bs =
  case classifyGitObject bs of
    Just BlobType -> fail $ show gop <> " is Git blob but expected Git tree"
    Just CommitType -> fail $ show gop <> " is Git commit but expected Git tree"
    Just TreeType -> pure . readTreeShas $ dropTreeHeader bs
    Just CollidedHash -> readTreeShas . dropTreeHeader <$> uniqBs gop cbs TreeType
    Nothing -> fail $ show gop <> " is not a Git tree object"

-- | just 'copyFile' is not possible due to trash after archive
saveCompressedBs :: PhoenixM m => FilePath -> L.ByteString -> m ()
saveCompressedBs fp bs =
  U.withBinaryFile fp U.WriteMode $ \h -> hPut h $ Z.compress bs

extractTree :: PhoenixExtractM m => GitPath Tree -> m ()
extractTree treeHash = do
  Tagged udr <- asks uberDir
  dd <- getDestDir
  withCompressedH (udr </> toFp treeHash) (copyTree treeHash) >>=
    mapM_ (copyTreeLinks dd)
  where
    copyTree trHash cTreeBs treeBs = do
      shas <- parseTreeObject trHash cTreeBs treeBs
      destDir <- getDestDir
      saveCompressedBs (destDir </> toFp trHash) treeBs
      pure shas
    getDestDir = (\(Tagged r) -> r </> ".git" </> "objects") <$> asks destGitDir
    copyTreeLinks destDir (dof, binSha) = do
      (Tagged udr) <- asks uberDir
      let shaP = binSha2Path binSha
          absSha = udr </> toFp shaP
          saveBlob = saveCompressedBs (destDir </> toFp shaP)
          saveTree bs = do
            saveBlob bs
            mapM_ (copyTreeLinks destDir) . readTreeShas $ dropTreeHeader bs
      withCompressedH absSha $ \cbs bs ->
        case classifyGitObject bs of
          Just BlobType
            | dof == File -> saveBlob bs
            | otherwise -> fail $ absSha <> " is not a GIT blob"
          Just TreeType
            | dof == Dir -> saveTree bs
            | otherwise -> fail $ absSha <> " is not a GIT tree"
          Just CollidedHash -> do
            ubs <- uniqBs shaP cbs (dofToGitObjType dof)
            case classifyGitObject ubs of
              Just BlobType -> saveBlob ubs
              Just TreeType -> saveTree ubs
              _ -> fail $ absSha <> " is not GIT tree nor GIT blob"
          _ -> fail $ absSha <> " is not a GIT tree nor GIT blob nor disambiguate file"
