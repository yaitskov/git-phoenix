module Lazy.Scope.GetContents where

import Control.Exception (ioError)
import Data.ByteString qualified as S
import Data.ByteString.Lazy.Internal
import Lazy.Scope.Type
import Relude hiding (Handle)
import System.IO.Error (mkIOError, illegalOperationErrorType)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Show (showsPrec)

hGet :: MonadIO m => Handle s -> Int -> LazyT s m S.ByteString
hGet (Handle h) n = lift (liftIO $ S.hGet h n)

hGetContentsOnlyN :: Int -> Handle s -> IO LByteString
hGetContentsOnlyN k (Handle h) = lazyRead
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h k
        if S.null c
          then return Empty
          else Chunk c <$> lazyRead

hGetContents :: MonadIO m => Handle s -> LazyT s m (Bs s)
hGetContents h = lift $ liftIO (Bs <$> hGetContentsOnlyN defaultChunkSize h)

hGetNonBlockingN :: Int -> Handle s -> Int -> IO LByteString
hGetNonBlockingN k (Handle h) n | n > 0= readChunks n
  where
    readChunks !i = do
        c <- S.hGetNonBlocking h (min k i)
        case S.length c of
            0 -> return Empty
            m -> do cs <- readChunks (i - m)
                    return (Chunk c cs)

hGetNonBlockingN _ _ 0 = return Empty
hGetNonBlockingN _ h n = illegalBufferSize h "hGetNonBlocking" n

illegalBufferSize :: Handle s -> String -> Int -> IO a
illegalBufferSize (Handle handle) fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []


hGetNonBlocking :: MonadIO m => Handle s -> Int -> LazyT s m (Bs s)
hGetNonBlocking h n = lift (liftIO $ Bs <$> hGetNonBlockingN defaultChunkSize h n)

hPutNonBlocking :: MonadIO m => Handle s -> Bs s -> LazyT s m (Bs s)
hPutNonBlocking _ (Bs Empty)           = pure (Bs Empty)
hPutNonBlocking bh@(Handle h) (Bs bs@(Chunk c cs)) = do
  c' <- lift (liftIO $ S.hPutNonBlocking h c)
  case S.length c' of
    l' | l' == S.length c -> hPutNonBlocking bh (Bs cs)
    0                     -> return $ Bs bs
    _                     -> return $ Bs (Chunk c' cs)
