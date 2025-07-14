module Lazy.Scope.Io where

import Control.DeepSeq
import Control.Monad.Trans.Class
import Data.ByteString.Lazy qualified as L
import Lazy.Scope.Bs
import Lazy.Scope.Type
import Relude
    ( ($), ($!),
      Bool(..),
      String,
      Integer,
      MonadIO(..),
      FilePath,
      IOMode,
      (<$>) )
import System.IO qualified as IO
import System.IO (SeekMode (..))
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.IO qualified as U

class WithFile a where
  -- withFile       :: a -> IOMode -> (Handle s -> LazyT s m r) -> LazyT s m r
  -- withTempFile   ::
  withBinaryFile :: (NFData r, MonadUnliftIO m) =>
    a -> IOMode -> (Handle s -> LazyT s m r) -> m r

  -- withBinaryTempFile ::
  -- withTempFileWithDefaultPermissions
  -- withBinaryTempFileWithDefaultPermissions

instance WithFile FilePath where
  {-# INLINE withBinaryFile #-}
  withBinaryFile fp mode cb =
    U.withBinaryFile fp mode (\h -> unLazy (cb (Handle h)))

hSeek :: MonadIO m => Handle s -> SeekMode -> Integer -> LazyT s m ()
hSeek (Handle h) sm n = LazyT $! U.hSeek h sm n
{-# INLINE hSeek #-}

hTell :: MonadIO m => Handle s -> LazyT s m Integer
hTell (Handle h) = lift $! U.hTell h
{-# INLINE hTell #-}

hSetPosn :: MonadIO m => HandlePosn s -> LazyT s m ()
hSetPosn (HandlePosn hpn) = lift (liftIO $! IO.hSetPosn hpn)

hGetPosn :: MonadIO m => Handle s -> LazyT s m (HandlePosn s)
hGetPosn (Handle h) = lift (liftIO $! HandlePosn <$> IO.hGetPosn h)

hFlush :: MonadIO m => Handle s -> LazyT s m ()
hFlush (Handle h) = lift (U.hFlush h)
-- hGetBuffering :: Handle s -> IO BufferMode
-- hSetBuffering :: Handle s -> BufferMode -> IO ()
-- hIsEOF :: Handle s -> IO Bool
-- hSetFileSize :: Handle s -> Integer -> IO ()
-- hFileSize :: Handle s -> IO Integer

-- hSetEncoding :: Handle s -> TextEncoding -> IO ()
-- hSetEncoding (Handle h) = IO.hSetEncoding h

-- hGetEncoding :: Handle s -> IO (Maybe TextEncoding)
-- hGetEncoding (Handle h) = IO.hGetEncoding h

-- hSetBinaryMode :: Handle -> Bool -> IO ()
-- hPutBuf :: Handle -> Ptr a -> Int -> IO ()
-- hGetBuf :: Handle -> Ptr a -> Int -> IO Int
-- hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
-- hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
-- hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
-- hSetNewlineMode :: Handle -> NewlineMode -> IO ()
-- hPrint :: Show a => Handle -> a -> IO ()
-- hGetChar :: Handle -> IO Char -- word8?
-- hReady :: Handle -> IO Bool
-- hWaitForInput :: Handle -> Int -> IO Bool
hShow :: MonadIO m => Handle s -> LazyT s m String
hShow (Handle h) = lift $ (liftIO $ IO.hShow h)

-- hGetEcho :: Handle -> IO Bool
-- hSetEcho :: Handle -> Bool -> IO ()
-- hIsTerminalDevice :: Handle -> IO Bool
hIsSeekable :: MonadIO m => (Handle s) -> LazyT s m Bool
hIsSeekable (Handle h) = lift (liftIO $ IO.hIsSeekable h)

-- hIsWritable :: Handle -> IO Bool
-- hIsReadable :: Handle -> IO Bool


-- -- class by value type
-- hPutStrLn :: Handle -> String -> IO ()
-- hPutStr :: Handle -> String -> IO ()

hPutBs :: MonadIO m => Handle s -> Bs s -> LazyT s m ()
hPutBs (Handle h) (Bs lbs) = LazyT (liftIO $ L.hPut h lbs)

-- hPutChar :: Handle -> Char -> IO ()
-- hLookAhead :: Handle -> IO Char
-- hGetLine :: Handle -> IO String
-- -- end of class
