module Data.Git.Phoenix.Io where

import Conduit
import Codec.Compression.Zlib qualified as Z
import Control.DeepSeq
import Data.ByteString.Lazy qualified as L
import Data.ByteString qualified as BS
import Data.Tagged (Tagged (..))
import Relude
import System.IO (openBinaryFile)
import UnliftIO.Exception qualified as U
import UnliftIO.IO qualified as U
import UnliftIO.QSem qualified as U

class HasInHandlesSem m where
  getInHandlesSem :: m U.QSem


instance (Monad m, HasInHandlesSem m) => HasInHandlesSem (ResourceT m) where
  getInHandlesSem = lift getInHandlesSem

withHandleX :: (NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  U.IOMode -> FilePath -> (Handle -> m a) -> m a
withHandleX mode fp a = do
  s <- getInHandlesSem
  U.bracket_ (U.waitQSem s) (U.signalQSem s) $
    -- withFile is not applicable because Handle might be closed twice
    -- https://github.com/haskell/bytestring/issues/707
    U.bracket (liftIO $ openBinaryFile fp mode)
      (\h -> whenM (U.hIsOpen h) $ U.hClose h) go
  where
    go h = do
      !r <- a h
      rnf r `seq` pure r

withHandle :: (NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath -> (Handle -> m a) -> m a
withHandle = withHandleX U.ReadMode

data Compressed

withCompressedH :: (NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath ->
  (Tagged Compressed L.ByteString -> L.ByteString -> m a) ->
  m a
withCompressedH fp a =
  withHandle fp $ \inH -> hGetContents inH >>= (\cbs -> a (Tagged cbs) $ Z.decompress cbs)

withCompressed :: (NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath -> (L.ByteString -> m a) -> m a
withCompressed fp a = withCompressedH fp (\_cbs bs -> a bs)

hGet :: MonadIO m => Handle -> Int -> m BS.ByteString
hGet h n = liftIO $ BS.hGet h n

hGetContents :: MonadIO m => Handle -> m L.ByteString
hGetContents h = liftIO $ L.hGetContents h

hPut :: MonadIO m => Handle -> L.ByteString -> m ()
hPut h bs = liftIO $ L.hPut h bs

readNumber :: MonadIO m => Int -> Int -> m Int
readNumber minVal maxVal = go
  where
    go = do
      s <- liftIO $ getLine
      case readMaybe $ toString s of
        Just n
          | n >= minVal && n <= maxVal ->
            pure n
          | otherwise -> do
            putStrLn "Value is out of range. Try again"
            go
        Nothing -> do
            putStrLn "Value is number. Try again"
            go
