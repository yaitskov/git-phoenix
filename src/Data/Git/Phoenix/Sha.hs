module Data.Git.Phoenix.Sha
  ( ComHash
  , hexToBin
  , shaToPath
  , binSha2Path
  , binSha2Str
  , parseSha1
  , showDigest
  , sha1
  , gitPath2Bs
  , cutGitPath
  ) where

import Data.Binary qualified as B
import Data.ByteArray.Encoding (Base(Base16), convertFromBase, convertToBase)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Char (isHexDigit)
import Data.Digest.Pure.SHA (Digest, SHA1State, showDigest, sha1)
import Data.Git.Phoenix.Object
import Data.List.Extra qualified as L
import Relude

type ComHash = Digest SHA1State

parseSha1 :: String -> Either String ComHash
parseSha1
  h | length (filter isHexDigit h) == 40 =
      fmap (B.decode . toLazy) $ convertFromBase Base16 (C.pack h)
    | otherwise = Left $ "Failed to parse SHA1: " <> h

fromRightEr :: Either String a -> a
fromRightEr = \case
  Right a -> a
  Left e -> error $ toText e

hexToBin :: LByteString -> LByteString
hexToBin = toLazy . fromRightEr . convertFromBase Base16 . toStrict

shaToPath :: String -> GitPath a
shaToPath = \case
  a:b:r -> GitPath $ a:b:'/':r
  o -> GitPath o

binSha2Path :: LByteString -> GitPath a
binSha2Path = shaToPath . binSha2Str

binSha2Str :: LByteString -> String
binSha2Str = C.unpack . convertToBase Base16 . toStrict

gitPath2Bs :: GitPath a -> LByteString
gitPath2Bs (GitPath fp) =
  case fp of
    a:b:'/':r -> hexToBin . L8.pack $ a:b:r
    _ -> error . toText $ "Bad GitPath: " <> fp

cutGitPath :: FilePath -> Maybe LByteString
cutGitPath fp =
  case L.takeEnd 41 fp of
    a:b:'/':r -> Just . hexToBin . L8.pack $ a:b:r
    _ -> Nothing
