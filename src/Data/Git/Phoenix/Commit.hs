module Data.Git.Phoenix.Commit where

import Data.ByteString.Lazy qualified as L
import Data.Word8 qualified as W
import Relude

extractField ::
  Word8 ->
  L.ByteString ->
  (L.ByteString -> (L.ByteString, L.ByteString)) ->
  L.ByteString ->
  (L.ByteString, L.ByteString)
extractField b field parseValue bs =
  case L.dropWhile (/= b) bs of
    "" -> ("", "")
    bs' ->
      if field `L.isPrefixOf` bs'
      then parseValue $ L.drop (2 + L.length field) bs'
      else extractField b field parseValue $ L.drop 1 bs'

extractParent :: L.ByteString -> (L.ByteString, L.ByteString)
extractParent =
  extractField
    (fromIntegral $ ord '\n')
    "\nparent "
    (L.span W.isHexDigit)


extractTreeHash :: L.ByteString -> (L.ByteString, L.ByteString)
extractTreeHash =
  extractField 0 "\0tree " (L.span W.isHexDigit)
