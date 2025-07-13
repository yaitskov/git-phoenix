module Data.Git.Phoenix.Io where

import Data.ByteString.Lazy qualified as L
import Data.Git.Phoenix.Prelude
import System.IO qualified as IO
import Lazy.Scope as S
import UnliftIO.IO qualified as U

class HasInHandlesSem m where
  getInHandlesSem :: m QSem

instance (Monad m, HasInHandlesSem m) => HasInHandlesSem (ResourceT m) where
  getInHandlesSem = lift getInHandlesSem

data Compressed

withHandleX :: (MonadUnliftIO m, HasInHandlesSem m) =>
  IOMode -> FilePath -> (Handle s -> LazyT s m a) -> LazyT s m a
withHandleX mode fp a = do
  s <- lift getInHandlesSem
  bracket_ (waitQSem s) (signalQSem s) $
    withBinaryFile fp mode a

withHandle :: (MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath -> (Handle s -> LazyT s m a) -> LazyT s m a
withHandle = withHandleX ReadMode

withCompressedH :: (MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath ->
  (Tagged Compressed (Bs s) -> Bs s -> LazyT s m a) ->
  LazyT s m a
withCompressedH fp a =
  withHandle ($(tr "/fp") fp) $ \inH -> hGetContents inH >>= (\cbs -> a (Tagged cbs) $ mapLbs decompress cbs)

withCompressed :: (MonadUnliftIO m, HasInHandlesSem m) =>
  FilePath -> (Bs s -> LazyT s m a) -> LazyT s m a
withCompressed fp a = withCompressedH fp (\_cbs bs -> a bs)

writeBinaryFile :: MonadUnliftIO m => FilePath -> IOMode -> (IO.Handle -> m ()) -> m ()
writeBinaryFile fp mode cb = U.withBinaryFile fp mode cb

hPutLbs :: MonadIO m => IO.Handle -> LByteString -> m ()
hPutLbs h bs = liftIO $ L.hPut h bs

-- | just 'copyFile' is not possible due to trash after archive
saveCompressedBs :: MonadUnliftIO m => FilePath -> Bs s -> LazyT s m ()
saveCompressedBs fp bs = do
  lift $ createDirectoryIfMissing False $ dropFileName fp
  withBinaryFile ($(tr "/fp") fp) WriteMode $ \h -> hPutBs h $ mapLbs compress bs

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
