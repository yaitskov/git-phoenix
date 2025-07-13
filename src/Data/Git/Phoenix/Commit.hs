module Data.Git.Phoenix.Commit where

import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word8 qualified as W
import Lazy.Scope as S
import Relude

type LbsPair = (LByteString, LByteString)

extractField ::
  Monad m =>
  Word8 -> Bs s -> (Bs s -> LazyT s m  (Bs s, Bs s)) -> Bs s -> LazyT s m (Bs s, Bs s)
extractField b field parseValue bs =
  case S.dropWhile (/= b) bs of
    "" -> pure ("", "")
    bs' ->
      ifM (field `S.isPrefixOfM` bs')
         (parseValue =<<  (`S.drop` bs') <$> S.lengthM field)
         (extractField b field parseValue $ S.drop 1 bs')

extractParent :: Monad m => Bs s -> LazyT s m (Bs s, Bs s)
extractParent =
  extractField W._lf "\nparent " (pure . S.span W.isHexDigit)

extractAuthor :: Monad m => Bs s -> LazyT s m (Bs s, Bs s)
extractAuthor =
  extractField W._lf "\nauthor " (pure . S.span (/= W._less))

extractCommitTs :: Monad m => Bs s -> LazyT s m (Maybe (Int64, Bs s))
extractCommitTs bs =
  extractField W._greater "> " (pure . S.span (/= W._lf)) bs >>= \case
    (tsBs, bs') ->
      unScope (bs2Scoped L8.readInt64 tsBs) >>= \case
        Nothing -> pure $ Nothing
        Just (epoch', spcTzBs) ->
          unScope (bs2Scoped L8.readInt64 $ S.dropWhile W.isSpace $ toBs spcTzBs) >>= \case
            Nothing -> pure $ Nothing
            Just (tz, _) ->
              let
                tzAbs = abs tz
                (tzH, tzM) = tzAbs `divMod` 100
              in
                pure $ Just (epoch' + (tzH * 3600 + tzM * 60) * signum tz, bs')

extractMessage :: Monad m => Bs s -> LazyT s m (Bs s)
extractMessage bs =
  extractField W._lf "\n\n" (pure . S.span (/= W._lf)) bs >>= \case
    (msgFirstLine, _) -> pure msgFirstLine

extractTreeHash :: Monad m => Bs s -> LazyT s m (Bs s, Bs s)
extractTreeHash =
  extractField 0 "\0tree " (pure . S.span W.isHexDigit)

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0
{-# INLINE epoch #-}

secondsToUtcTime :: Integer -> UTCTime
secondsToUtcTime x = addUTCTime (realToFrac $ picosecondsToDiffTime (x * 1000000000000)) epoch
