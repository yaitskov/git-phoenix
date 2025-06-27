module Data.Git.Phoenix where

import Codec.Compression.Zlib qualified as Z
import Hexdump (simpleHex)
import Relude

foo :: IO ()
foo = do
  print $ Z.compress "Hello World"
  print . Z.decompress $ Z.compress "Hello World"
