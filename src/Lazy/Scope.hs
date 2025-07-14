module Lazy.Scope
  ( LazyT
  , Handle
  , HandlePosn
  , Bs
  , toBs
  , Scoped
  -- , collapse
  -- , collapse_
  -- , collapseScoped
  , WithFile (..)
  , hSeek
  , hTell
  , hSetPosn
  , hGetPosn
  , hFlush
  , hShow
  , hIsSeekable
  , hPutBs
  , isPrefixOfM
  , toLbs
  , unScope
  , lengthM
  , take
  , drop
  , dropWhile
  , span
  , mapLbs
  , bs2Scoped
  , scoped2Bs
  , unpack8
  , hGet
  , hGetContents
  , hGetNonBlocking
  , hPutNonBlocking
  ) where

import Control.DeepSeq
import Control.Monad.Trans.Class
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Lazy.Scope.Type
import Lazy.Scope.GetContents
import Relude
    ( otherwise, ($), ($!),
      Monad,
      Applicative(pure),
      Bool(..),
      String,
      Int64,
      Integer,
      Word8,
      MonadIO(..),
      LByteString,
      FilePath,
      IOMode,
      (<$>) )
import System.IO qualified as IO
import System.IO (SeekMode (..))
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.IO qualified as U


-- collapse :: (NFData a, MonadUnliftIO m) => (forall s. LazyT s m a) -> m a
-- collapse (LazyT m) = do
--   -- @rnf r `seq` pure r@ is not working
--   !r <- m
--   case rnf r of
--     () -> pure r

-- collapse_ :: MonadUnliftIO m => (forall s. LazyT s m ()) -> m ()
-- collapse_ (LazyT m) = do
--   -- @rnf r `seq` pure r@ is not working
--   !r <- m
--   case r of
--     () -> pure r

-- collapseScoped :: (NFData a, MonadUnliftIO m) => (forall s. LazyT s m (Scoped s a)) -> m a
-- collapseScoped (LazyT m) = do
--   -- @rnf r `seq` pure r@ is not working
--   !(Scoped !r) <- m
--   case rnf r of
--     () -> pure r

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

isPrefixOfM :: Applicative m => Bs s -> Bs s -> LazyT s m Bool
isPrefixOfM (Bs a) (Bs b)
  | a `L.isPrefixOf` b = pure True
  | otherwise = pure False
{-# INLINE isPrefixOfM #-}

take :: Int64 -> Bs s -> Bs s
take n (Bs lbs) = Bs $ L.take n lbs
{-# INLINE take #-}

lengthM :: Applicative m => Bs s -> LazyT s m Int64
lengthM (Bs lbs) = case rnf l of () -> pure l
  where
    l = L.length lbs
{-# INLINE lengthM #-}

drop :: Int64 -> Bs s -> Bs s
drop n (Bs lbs) = Bs $ L.drop n lbs
{-# INLINE drop #-}

dropWhile :: (Word8 -> Bool) -> Bs s -> Bs s
dropWhile p (Bs lbs) = Bs $ L.dropWhile p lbs
{-# INLINE dropWhile #-}

span :: (Word8 -> Bool) -> Bs s -> (Bs s, Bs s)
span p (Bs lbs) =
  case L.span p lbs of
    (x, y) -> (Bs x, Bs y)

unScope :: (NFData a, Monad m) => Scoped s a -> LazyT s m a
unScope (Scoped !a) =
  case rnf a of
    () -> pure a

bs2Scoped :: (LByteString -> a) -> Bs s -> Scoped s a
bs2Scoped f (Bs lbs) = Scoped $ f lbs

scoped2Bs :: (a -> LByteString) -> Scoped s a -> Bs s
scoped2Bs f (Scoped a) = Bs $ f a

mapLbs :: (LByteString -> LByteString) -> Bs s -> Bs s
mapLbs f (Bs lbs) = Bs $ f lbs

unpack8 :: Monad m => Bs s -> LazyT s m String
unpack8 (Bs lbs) = pure (L8.unpack lbs)

toLbs :: Monad m => Bs s -> LazyT s m LByteString
toLbs (Bs a) =
  case rnf a of
    () -> pure a
