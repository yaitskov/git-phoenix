{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Git.Phoenix.Object where

import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Git.Phoenix.Prelude
import Lazy.Scope qualified as S


data GitObjType = CommitType | TreeType | BlobType | CollidedHash deriving (Show, Eq)

data GitObjTypeG = Commit | Tree deriving (Show, Eq)

-- | Path relative to .git/objects or uber dir
newtype GitPath (t :: GitObjTypeG) = GitPath { toFp :: FilePath } deriving (Show, Eq, NFData)

toCommitSha :: GitPath t -> LByteString
toCommitSha (GitPath p) = L8.pack $ filter (/= '/') p

classifyGitObject :: Monad m => Bs s -> LazyT s m (Maybe GitObjType)
classifyGitObject bs =
  condM
    [ (blob `S.isPrefixOf` bs, pure $ pure BlobType)
    , (tree `S.isPrefixOf` bs, pure $ pure TreeType)
    , (commit `S.isPrefixOf` bs, pure $ pure CommitType)
    , (toBs disambiguate `S.isPrefixOf` bs, pure $ pure CollidedHash)
    ]
    (pure Nothing)

commit, tree, blob :: Bs s
commit = "commit "
blob = "blob "
tree = "tree "

disambiguate :: LByteString
disambiguate = "disambigate "

gitObjectP :: Monad m => Bs s -> LazyT s m Bool
gitObjectP bs =
  classifyGitObject bs >>= pure . \case
    Nothing -> False
    Just CollidedHash -> False
    Just _ -> True

compressedDisambiguate :: LByteString
compressedDisambiguate = compressWith params disambiguate
  where
    params = defaultCompressParams { compressLevel = CompressionLevel 0 }

compressedDisambiguateBs :: Bs s
compressedDisambiguateBs = toBs compressedDisambiguate

compressedDisambiguateLen :: Int64
compressedDisambiguateLen = L.length compressedDisambiguate

encodedIntLen :: Int64
encodedIntLen = L.length . B.encode $ L.length ""
