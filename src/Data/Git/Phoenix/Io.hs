module Data.Git.Phoenix.Io where

import Data.ByteString.Lazy qualified as L
import Data.ByteString qualified as BS
import Data.Git.Phoenix.Prelude
import System.IO (openBinaryFile)

class HasInHandlesSem m where
  getInHandlesSem :: m QSem

instance (Monad m, HasInHandlesSem m) => HasInHandlesSem (ResourceT m) where
  getInHandlesSem = lift getInHandlesSem

withHandleX :: (NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  IOMode -> FilePath -> (Handle -> m a) -> m a
withHandleX mode fp a = do
  s <- getInHandlesSem
  bracket_ (waitQSem s) (signalQSem s) $
    -- withFile is not applicable because Handle might be closed twice
    -- https://github.com/haskell/bytestring/issues/707
    bracket (liftIO $ openBinaryFile fp mode)
      (\h -> whenM (hIsOpen h) $ hClose h) go
  where
    go h = do
      !r <- a h
      rnf r `seq` pure r

withHandle :: (NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath -> (Handle -> m a) -> m a
withHandle = withHandleX ReadMode

data Compressed

withCompressedH :: (NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath ->
  (Tagged Compressed LByteString -> LByteString -> m a) ->
  m a
withCompressedH fp a =
  withHandle ($(tr "!/fp") fp) $ \inH -> hGetContents inH >>= (\cbs -> a (Tagged cbs) $ decompress cbs)

withCompressed :: (HasCallStack, NFData a, MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath -> (HasCallStack => L.ByteString -> m a) -> m a
withCompressed fp a = withCompressedH fp (\_cbs bs -> a bs)

hGet :: MonadIO m => Handle -> Int -> m ByteString
hGet h n = liftIO $ BS.hGet h n

hGetContents :: MonadIO m => Handle -> m LByteString
hGetContents h = liftIO $ L.hGetContents h

hPut :: MonadIO m => Handle -> LByteString -> m ()
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
