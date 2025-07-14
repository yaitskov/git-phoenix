{-# LANGUAGE UndecidableInstances #-}
module Lazy.Scope.Type where

import Relude hiding (Handle)
import System.IO qualified as IO
import UnliftIO (MonadUnliftIO (..))

newtype Handle s = Handle IO.Handle deriving (Show, Eq)
newtype HandlePosn s = HandlePosn IO.HandlePosn deriving (Show, Eq)

newtype LazyT s m a = LazyT { unLazy :: m a }

instance Functor m => Functor (LazyT s m) where
  {-# INLINE fmap #-}
  fmap f (LazyT m) = LazyT (fmap f m)

instance Applicative m => Applicative (LazyT s m) where
  pure a = LazyT (pure a)
  {-# INLINE pure #-}
  liftA2 f (LazyT ma) (LazyT mb) = LazyT (liftA2 f ma mb)
  {-# INLINE liftA2 #-}

instance Monad m => Monad (LazyT s m) where
  (>>=) :: LazyT s m a -> (a -> LazyT s m b) -> LazyT s m b
  LazyT m1 >>= fm2 = LazyT (m1 >>= \a -> unLazy (fm2 a))
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (LazyT s m) where
  fail s = LazyT (fail s)
instance MonadTrans (LazyT s) where
  lift m = LazyT m

instance MonadReader r m => MonadReader r (LazyT s m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f (LazyT m) = LazyT (local f m)

instance MonadState s' m => MonadState s' (LazyT s m) where
  get = lift get
  put s = lift (put s)

instance MonadIO m => MonadIO (LazyT s m) where
  {-# INLINE liftIO #-}
  liftIO io = LazyT (liftIO io)

instance MonadUnliftIO m => MonadUnliftIO (LazyT s m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    LazyT $ withRunInIO $ \run -> inner (run . unLazy)
