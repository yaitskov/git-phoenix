module Data.Git.Phoenix where

import Codec.Compression.Zlib qualified as Z
import Data.ByteString.Lazy qualified as L
import Hexdump (simpleHex)
import Relude
import Data.Digest.Pure.SHA

blobPath :: FilePath
blobPath = "/home/dan/pro/git-phoenix/.git/objects/04/327561a0005dc9ac2742e88ff4d059bf122ca2"

foo :: IO ()
foo = do
  print . showDigest . sha1 . Z.decompress . (<> "aoeu") =<< L.readFile blobPath
  print . L.take 42 . Z.decompress . L.take 48 =<< L.readFile blobPath
  print $ simpleHex $ toStrict $ Z.compressWith
    (Z.defaultCompressParams { Z.compressLevel = Z.CompressionLevel 0 })
    "Hello World"
  print . Z.decompress . (<> "aoeu") =<< L.readFile blobPath
  print . simpleHex . toStrict =<< L.readFile blobPath
  print $ simpleHex $ toStrict $ Z.compress "Hello World"
  print . Z.decompress $ Z.compress "Hello World"
