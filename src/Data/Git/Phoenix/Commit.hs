module Data.Git.Phoenix.Commit where

import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
-- import Data.Time.Format.Internal
import Data.Word8 qualified as W
import Relude

type LbsPair = (LByteString, LByteString)

extractField ::
  Word8 -> LByteString -> (LByteString -> LbsPair) -> LByteString -> LbsPair
extractField b field parseValue bs =
  case L.dropWhile (/= b) bs of
    "" -> ("", "")
    bs' ->
      if field `L.isPrefixOf` bs'
      then parseValue $ L.drop (L.length field) bs'
      else extractField b field parseValue $ L.drop 1 bs'

extractParent :: L.ByteString -> LbsPair
extractParent =
  extractField W._lf "\nparent " (L.span W.isHexDigit)

extractAuthor :: LByteString -> LbsPair
extractAuthor =
  extractField W._lf "\nauthor " (L.span (/= W._less))

extractCommitTs :: LByteString -> Maybe (Int64, LByteString)
extractCommitTs bs =
  case extractField W._greater "> " (L.span (/= W._lf)) bs of
    (tsBs, bs') ->
      case L8.readInt64 tsBs of
        Nothing -> Nothing
        Just (epoch', spcTzBs) ->
          case L8.readInt64 $ L.dropWhile W.isSpace spcTzBs of
            Nothing -> Nothing
            Just (tz, _) ->
              let
                tzAbs = abs tz
                (tzH, tzM) = tzAbs `divMod` 100
              in
                Just (epoch' + (tzH * 3600 + tzM * 60) * signum tz, bs')

extractMessage :: LByteString -> LByteString
extractMessage bs =
  case extractField W._lf "\n\n" (L.span (/= W._lf)) bs of
    (msgFirstLine, _) -> msgFirstLine

extractTreeHash :: LByteString -> LbsPair
extractTreeHash =
  extractField 0 "\0tree " (L.span W.isHexDigit)

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0
{-# INLINE epoch #-}

secondsToUtcTime :: Integer -> UTCTime
secondsToUtcTime x = addUTCTime (realToFrac $ picosecondsToDiffTime (x * 1000000000000)) epoch
