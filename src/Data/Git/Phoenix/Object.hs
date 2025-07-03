module Data.Git.Phoenix.Object where

import Codec.Compression.Zlib qualified as Z
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as L
import Relude

data GitObjType = CommitType | TreeType | BlobType | CollidedHash deriving (Show, Eq)

data GitObjTypeG = Commit | Tree deriving (Show, Eq)

-- | Path relative to .git/objects or uber dir
newtype GitPath (t :: GitObjTypeG) = GitPath { toFp :: FilePath } deriving (Show, Eq, NFData)

classifyGitObject :: L.ByteString -> Maybe GitObjType
classifyGitObject bs
  | blob `L.isPrefixOf` bs = pure BlobType
  | tree `L.isPrefixOf` bs = pure TreeType
  | commit `L.isPrefixOf` bs = pure CommitType
  | disambiguate `L.isPrefixOf` bs = pure CollidedHash
  | otherwise = Nothing

commit, tree, blob, disambiguate :: L.ByteString
disambiguate = "disambigate "
commit = "commit "
blob = "blob "
tree = "tree "

gitObjectP :: L.ByteString -> Bool
gitObjectP bs =
  case classifyGitObject bs of
    Nothing -> False
    Just CollidedHash -> False
    Just _ -> True

compressedDisambiguate :: L.ByteString
compressedDisambiguate =
  Z.compressWith
    (Z.defaultCompressParams { Z.compressLevel = Z.CompressionLevel 0 })
    disambiguate

encodedIntLen :: Int64
encodedIntLen = L.length . B.encode $ L.length ""
