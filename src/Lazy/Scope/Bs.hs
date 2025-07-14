module Lazy.Scope.Bs where

import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Lazy.Scope.Type
import Relude hiding (Handle)

newtype Bs s = Bs LByteString deriving (Show, Eq, Ord, NFData, Semigroup, IsString, Monoid)

toBs :: LByteString -> Bs s
toBs = Bs
{-# INLINE toBs #-}

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

mapLbs :: (LByteString -> LByteString) -> Bs s -> Bs s
mapLbs f (Bs lbs) = Bs $ f lbs

unpack8 :: Monad m => Bs s -> LazyT s m String
unpack8 (Bs lbs) = pure (L8.unpack lbs)

toLbs :: Monad m => Bs s -> LazyT s m LByteString
toLbs (Bs a) =
  case rnf a of
    () -> pure a
