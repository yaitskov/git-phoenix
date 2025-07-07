module Data.Git.Phoenix.ShaCollision where

import Data.List.NonEmpty (groupWith)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Git.Phoenix.App
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude


disambiguateByPair :: PhoenixM m => GitObjType -> [FilePath] -> m [FilePath]
disambiguateByPair tt links =
  fmap (snd . head) . groupWith fst . sort . catMaybes <$> mapM go links
  where
    go l = do
      withCompressed l $ \bs -> do
        case classifyGitObject bs of
          Just x | x == tt -> pure $ Just (bs, l)
                 | otherwise -> pure Nothing
          Nothing -> pure Nothing

uniqBs :: PhoenixExtractM m =>
  GitPath x ->
  Tagged Compressed LByteString ->
  GitObjType ->
  m FilePath
uniqBs ambiHash cbs expectedGitObjType = do
  case parseFileLinks cbs of
    [_] -> fail $ show cbs <> " is not ambiguous"
    [] -> fail $ show cbs <> " is emply list"
    links -> do
      disLinks <- disambiguateByPair expectedGitObjType $ fmap L8.unpack links
      case disLinks of
        [] -> fail $ show cbs <> " is emply list after dis"
        [a] -> pure a
        uniqLinks -> chooseOneLink uniqLinks
  where
    chooseOneLink links = do
      forM_ (zip [0 :: Int ..] links) $ \(i, l) -> putStrLn $ printf "%4d) %s" i l
      putStrLn "-----------------------------------------------------------"
      putStrLn $ "Enter link number to disambiguate SHA " <> toFp ambiHash <> " of " <> show expectedGitObjType
      i <- readNumber 0 (length links - 1)
      case links !? i of
        Nothing -> fail $ "Link index out of range: " <> show i <> " for " <> show (length links)
        Just l -> pure l

parseFileLinks :: Tagged Compressed LByteString -> [LByteString]
parseFileLinks (Tagged preCbs) = go cbs
  where
    go bs =
      case L.splitAt encodedIntLen bs of
        ("", _) -> []
        (binLen, bs') ->
          case B.decodeOrFail binLen of
            Right (_, _, fsLinkLen) ->
              case L.splitAt fsLinkLen bs' of
                (fsLinkBs, bs'')
                  | L.length fsLinkBs == fsLinkLen ->
                    fsLinkBs : go bs''
                  | otherwise ->
                      error $ "Expected link len " <> show fsLinkLen
                        <> " but got " <> show (L.length fsLinkBs)
            Left e ->
              error $ "List of files with collided SHA is corrupted (error: "
                <> show e <> ") near: "  <> show bs

    cbs = L.drop (L.length compressedDisambiguate) preCbs
