module Data.Git.Phoenix.Commit where

import Data.ByteString.Lazy qualified as L
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
  extractField
    (fromIntegral $ ord '\n')
    "\nparent "
    (L.span W.isHexDigit)


extractTreeHash :: L.ByteString -> LbsPair
extractTreeHash =
  extractField 0 "\0tree " (L.span W.isHexDigit)
