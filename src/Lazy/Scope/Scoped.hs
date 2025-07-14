module Lazy.Scope.Scoped where

import Lazy.Scope.Bs
import Lazy.Scope.Type
import Relude hiding (Handle)

newtype Scoped s a = Scoped a deriving (Show, Eq, NFData, Functor)

instance Applicative (Scoped s) where
  pure = Scoped
  {-# INLINE pure #-}
  liftA2 f (Scoped a) (Scoped b) = Scoped $ f a b
  {-# INLINE liftA2 #-}


unScope :: (NFData a, Monad m) => Scoped s a -> LazyT s m a
unScope (Scoped !a) =
  case rnf a of
    () -> pure a

bs2Scoped :: (LByteString -> a) -> Bs s -> Scoped s a
bs2Scoped f (Bs lbs) = Scoped $ f lbs

scoped2Bs :: (a -> LByteString) -> Scoped s a -> Bs s
scoped2Bs f (Scoped a) = Bs $ f a
