module Data.Git.Phoenix.CommitSearch where

import Data.ByteString.Lazy qualified as L
import Data.Git.Phoenix.App
import Data.Git.Phoenix.Io
import Relude
data Commit
  = Commit
    { message :: L.ByteString
    , file :: FilePath
    -- , ts :: UTCTime
    -- , parent :: Maybe (Digest SHA1State)
    } deriving (Show, Generic)

instance NFData Commit

extractAuthor :: L.ByteString -> (L.ByteString, L.ByteString)
extractAuthor bs =
  case L.dropWhile (/= fromIntegral (ord '\n')) bs of
    "" -> ("", "")
    bs' ->
      if author `L.isPrefixOf` bs'
      then L.break (== fromIntegral (ord '<')) $ L.drop (L.length author) bs'
      else extractAuthor $ L.drop 1 bs'
  where
    author = "\nauthor "

extractMessage :: L.ByteString -> L.ByteString
extractMessage bs =
  case L.uncons bs of
    Just (10, bs') ->
      case L.uncons bs' of
        Just (10, bs'') -> bs''
        Just (_, bs'') -> extractMessage bs''
        Nothing -> ""
    Just (_, bs') -> extractMessage bs'
    Nothing -> ""

filterCommit :: PhoenixM m => L.ByteString -> FilePath -> m (Maybe Commit)
filterCommit targetAuthor fp = withCompressed fp go
  where
    go bs
      | "commit " `L.isPrefixOf` bs =
        case extractAuthor bs of
         (comAuthor, rest)
           | targetAuthor `L.isPrefixOf` comAuthor ->
             pure . Just $ Commit (extractMessage rest) fp
           | otherwise -> pure Nothing
      | otherwise = pure Nothing

filterGitObjType :: PhoenixM m => L.ByteString -> FilePath -> m (Maybe ())
filterGitObjType goType fp = withCompressed fp go
  where
    go bs
      | goType `L.isPrefixOf` bs = pure $ Just ()
      | otherwise = pure Nothing
