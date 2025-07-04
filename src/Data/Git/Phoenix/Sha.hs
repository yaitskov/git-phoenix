module Data.Git.Phoenix.Sha
  ( ComHash
  , hexToBin
  , shaToPath
  , binSha2Path
  , parseSha1
  , showDigest
  , sha1
  ) where

import Data.Binary qualified as B
import Data.ByteArray.Encoding (Base(Base16), convertFromBase, convertToBase)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as L
import Data.Char (isHexDigit)
import Data.Digest.Pure.SHA (Digest, SHA1State, showDigest, sha1)
import Data.Git.Phoenix.Object
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

hexToBin :: L.ByteString -> L.ByteString
hexToBin = toLazy . fromRightEr . convertFromBase Base16 . toStrict

shaToPath :: String -> GitPath a
shaToPath = \case
  a:b:r -> GitPath $ a:b:'/':r
  o -> GitPath o

binSha2Path :: L.ByteString -> GitPath a
binSha2Path = shaToPath . C.unpack . convertToBase Base16 . toStrict
