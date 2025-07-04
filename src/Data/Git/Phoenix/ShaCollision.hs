module Data.Git.Phoenix.ShaCollision where

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

uniqBs :: PhoenixExtractM m =>
  GitPath x ->
  Tagged Compressed LByteString ->
  GitObjType ->
  m LByteString
uniqBs ambiHash (Tagged preCbs) expectedGitObjType = do
  case parseFileLinks cbs of
    [_] -> fail $ show cbs <> " is not ambiguous"
    [] -> fail $ show cbs <> " is emply list"
    links -> chooseOneLink $ fmap L8.unpack links
  where
    chooseOneLink links = do
      forM_ (zip [0 :: Int ..] links) $ \(i, l) -> putStrLn $ printf "%4d) %s" i l
      putStrLn "-----------------------------------------------------------"
      putStrLn $ "Enter link number to disambiguate SHA " <> toFp ambiHash <> " of " <> show expectedGitObjType
      i <- pure $ length links - 1  -- readNumber 0 (length links - 1)
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
