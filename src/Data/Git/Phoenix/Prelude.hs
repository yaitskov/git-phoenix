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
import Data.Word8 as X (isHexDigit)
import Lazy.Scope as X (LazyT, Scoped, Bs, unScope, toLbs, condM, toBs, bs2Scoped)
import Relude as X hiding (Handle)
import System.FilePath as X ((</>), dropFileName, splitFileName, makeRelative)
import System.Time.Extra as X
import Text.Printf as X
import UnliftIO.QSem as X (QSem, newQSem, signalQSem, waitQSem)
import UnliftIO.Exception as X (bracket, bracket_, catch)
import UnliftIO.Directory as X
  ( createFileLink, createDirectoryIfMissing
  , pathIsSymbolicLink, makeAbsolute, createDirectory
  , getSymbolicLinkTarget, removeFile, copyFile
  , doesFileExist, doesDirectoryExist, listDirectory
  )
