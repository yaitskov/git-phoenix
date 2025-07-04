module Data.Git.Phoenix.ShaCollision where

import Data.List.NonEmpty (groupWith)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Git.Phoenix.App
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude

      -- read list of colliding files filter out non tree objects
      -- dedup left by pair

      -- check previous choice
      -- ask user choice (specific number, random, RANDOM - for all)
      -- remember choice if not random
      -- do readCommitObject recursively

disambiguateByPair :: PhoenixExtractM m => GitObjType -> [FilePath] -> m [FilePath]
disambiguateByPair tt links =
  fmap (snd . head) . groupWith fst . sort . catMaybes <$> mapM go links
  where
    go l = do
      withCompressed l $ \bs -> do
        case classifyGitObject bs of
          Just x | x == tt -> pure $ Just (bs, l)
                 | otherwise -> pure Nothing
          Nothing -> pure Nothing

  --   go headBs headLink = do
  -- case links of
  --   [a] -> pure [a]
  --   [] -> pure []
  --   h1 : h2 : links' ->
  --     cmpCompressed


uniqBs :: PhoenixExtractM m =>
  GitPath x ->
  Tagged Compressed LByteString ->
  GitObjType ->
  m LByteString
uniqBs ambiHash (Tagged preCbs) expectedGitObjType = do
  case parseFileLinks cbs of
    [_] -> fail $ show cbs <> " is not ambiguous"
    [] -> fail $ show cbs <> " is emply list"
    links -> do
      disLinks <- disambiguateByPair expectedGitObjType $ fmap L8.unpack links
      case disLinks of
        [] -> fail $ show cbs <> " is emply list after dis"
        [a] -> do
          putStrLn $ "Filtered links " <> show (length links - 1)
          withCompressed a pure
        uniqLinks -> do
          putStrLn $ "Filtered links " <> show (length links - length uniqLinks)
          chooseOneLink uniqLinks
  where
    chooseOneLink links = do
      forM_ (zip [0 :: Int ..] links) $ \(i, l) -> putStrLn $ printf "%4d) %s" i l
      putStrLn "-----------------------------------------------------------"
      putStrLn $ "Enter link number to disambiguate SHA " <> toFp ambiHash <> " of " <> show expectedGitObjType
      i <- readNumber 0 (length links - 1) -- pure $ length links - 1  --
      case links !? i of
        Nothing -> fail $ "Link index out of range: " <> show i <> " for " <> show (length links)
        Just l -> withCompressed l pure
    cbs = L.drop (L.length compressedDisambiguate) preCbs
    parseFileLinks bs =
      case L.splitAt encodedIntLen bs of
        ("", _) -> []
        (binLen, bs') ->
          case B.decodeOrFail binLen of
            Right (_, _, fsLinkLen) ->
              case L.splitAt fsLinkLen bs' of
                (fsLinkBs, bs'')
                  | L.length fsLinkBs == fsLinkLen ->
                    fsLinkBs : parseFileLinks bs''
                  | otherwise ->
                      error $ "Expected link len " <> show fsLinkLen
                        <> " but got " <> show (L.length fsLinkBs)
            Left e ->
              error $ "List of files with collided SHA is corrupted (error: "
                <> show e <> ") near: "  <> show bs


-- disambiguateCommit :: PhoenixM m => String -> Tagged InDir FilePath -> m FilePath
-- disambiguateCommit shaPrefix (Tagged uberRepoDir) =
--   case filter isHexDigit $ fmap toLower shaPrefix of
--     a : b : r -> do
--       matchingShas <- filter (r `isPrefixOf`) <$> U.listDirectory (uberRepoDir </> [a,b])
--       case matchingShas of
--         [shaSuffix] ->
--           case parseSha1 $ a:b:shaSuffix of
--             Right h -> pure h
--             Left er -> fail er
--         [] ->
--           fail $ "No SHA in uber repo [" <> uberRepoDir <> "] matching prefix: " <> shaPrefix
--         ambiSha ->
--           fail $ "SHA prefix " <> shaPrefix <> " is ambiguous and matching: "
--           <> lines (fmap (a:b:) ambiSha)
--     shortPrefix -> fail $ "Root SHA prefix is too short: " <> shortPrefix
