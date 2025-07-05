module Data.Git.Phoenix.Prelude (module X) where

import Control.Concurrent as X (getNumCapabilities)
import Codec.Compression.Zlib as X
  ( CompressionLevel (..), DecompressError (..)
  , CompressParams (..), DecompressParams (..)
  , compress, decompress
  , compressWith, decompressWith, defaultCompressParams
  , defaultDecompressParams
  )
import Conduit as X
  ( MonadUnliftIO, ResourceT, ConduitT, foldMC
  , mapMC, concatC, sourceDirectoryDeep
  )
import Control.DeepSeq as X
import Debug.TraceEmbrace as X hiding (a)
import Data.Conduit as X (runConduitRes, (.|))
import Data.List as X ((!?))
import Data.Tagged as X (Tagged (..), untag)
import Relude as X
import System.FilePath as X ((</>), dropFileName)
import System.Time.Extra as X
import Text.Printf as X
import UnliftIO.IO as X (withBinaryFile, hIsOpen, hClose)
import UnliftIO.QSem as X (QSem, newQSem, signalQSem, waitQSem)
import UnliftIO.Exception as X (bracket, bracket_, catch)
import UnliftIO.Directory as X
  ( createFileLink, createDirectoryIfMissing
  , pathIsSymbolicLink, makeAbsolute, createDirectory
  , getSymbolicLinkTarget, removeFile, copyFile
  )
