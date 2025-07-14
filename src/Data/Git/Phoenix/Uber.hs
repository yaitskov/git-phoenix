-- | Collect links to GIT objects under 1 directory
module Data.Git.Phoenix.Uber where

import Control.Lens ((%~), _2)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs (InDir)
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Sha
import Data.List qualified as I
import Data.Map.Strict qualified as M
import Lazy.Scope as S

type ShaDedupMap = M.Map ComHash Int

data GitObject
  = GitObject
    { gobHash :: !ComHash
    , gobOrigin :: !FilePath -- abs
    }
    deriving (Show, Eq, Generic)

instance NFData GitObject

gitObjectFilePath :: GitObject -> FilePath
gitObjectFilePath = uncurry (</>) . I.splitAt 2 . showDigest . gobHash

mkGitObject :: forall m. PhoenixM m => FilePath -> m (Maybe GitObject)
mkGitObject fp = go
  where
    go :: m (Maybe GitObject)
    go =
      withHandle fp $ \inH -> unScope =<< do
        magicBs <- S.hGet inH 2
        if zlibP magicBs
          then do
            (`catch` skipCorruptedFile) $ do
              headerBs <- (toLazy magicBs <>) . toLazy <$> S.hGet inH 510
              ifM (gitObjectP . toBs $ decompress headerBs)
                (S.bs2Scoped (Just . (`GitObject` fp) . sha1 . decompress) . (toBs headerBs <>) <$> S.hGetContents inH)
                (pure (pure Nothing))
          else pure (pure Nothing)

    skipCorruptedFile (_ :: DecompressError) = do
      liftIO $ $(trIo "Skip corrupted file/fp")
      pure (pure Nothing)

    zlibNoCompression = "\x0078\x0001"
    zlibDefaultCompression = "\x0078\x009C"
    zlibBestCompression = "\x0078\x00DA"

    zlibP bs =
      zlibNoCompression == bs ||
      zlibDefaultCompression == bs ||
      zlibBestCompression == bs

findGitObjects :: PhoenixCoCon m => FilePath -> ConduitT i GitObject m ()
findGitObjects photorecOutDir =
  sourceDirectoryDeep False photorecOutDir
  .| mapMC mkGitObject
  .| concatC

alrr :: Monad m => (x -> m y) -> (x, z) -> m z
alrr f (a, !r) = f a >> pure r

replaceSymLinkWithDisambiguate :: MonadUnliftIO m => FilePath -> GitObject -> m ()
replaceSymLinkWithDisambiguate uberGob gob = do
  firstGobOrigin <- L8.pack <$> getSymbolicLinkTarget uberGob
  removeFile uberGob
  S.withBinaryFile uberGob WriteMode $ \oh ->
    hPutBs oh . mconcat $ (compressedDisambiguateBs : fmap toBs [
                            B.encode $ L.length firstGobOrigin
                           , firstGobOrigin
                           , B.encode $ L.length gobPacked
                           , gobPacked
                           ])
  where
    gobPacked = L8.pack $ gobOrigin gob

appendPathToUberGob :: MonadUnliftIO m => FilePath -> GitObject -> m ()
appendPathToUberGob uberGob gob =
  writeBinaryFile uberGob AppendMode $ \oh ->
    hPutLbs oh $ gobLen <> gobPacked
  where
    gobPacked = L8.pack $ gobOrigin gob
    gobLen = B.encode $ L.length gobPacked

storeGitObject :: PhoenixUberM m => (ShaDedupMap, Int, Int) -> GitObject -> m (ShaDedupMap, Int, Int)
storeGitObject (dedupMap, !countDown, !mapSize) gob = do
  when (countDown == 0) $ do
    putStrLn $ "GIT objects found: " <> show mapSize
  -- todo: dynamic init countDown value to keep print period ~ a few seconds
  -- usb stick is slow, but SSD is fast
  (,if countDown <= 0 then 10000 else countDown - 1, mapSize + 1)
    <$> (alrr writeGitObject $ M.insertLookupWithKey (\_h -> (+)) (gobHash gob) 1 dedupMap)
  where
   gobPath = gitObjectFilePath gob
   writeGitObject = \case
     Nothing -> do
       dod <- asks $ untag . destObjectDir
       createDirectoryIfMissing False (dod </> dropFileName gobPath)
       createFileLink (gobOrigin gob) (dod </> gobPath)
     Just _dedupSuffix -> do
       dod <- asks $ untag . destObjectDir
       let uberGob = dod </> gobPath
       pathIsSymbolicLink uberGob >>= \case
         True ->
           replaceSymLinkWithDisambiguate uberGob gob
         False ->
           appendPathToUberGob uberGob gob

recoverFrom :: PhoenixUberM m => Tagged InDir FilePath -> m ()
recoverFrom (Tagged photorecOutDir) =
  duration (makeAbsolute photorecOutDir >>= go) >>= reportCollisions
  where
    go absInDir =
      (_2 %~ M.elems) . (\(m, _, objectsStored) -> (objectsStored, m)) <$>
        runConduitRes
          (  findGitObjects absInDir
          .| foldMC storeGitObject (mempty, 10, 0)
          )
    reportCollisions = \case
      (_, (_, [])) ->
        putStrLn $ "Dir [" <> photorecOutDir <> "] doesn't have Git files"
      (durSecs, (objectStored, collisions)) -> do
        putStrLn . printf "Duration: %s" $ showDuration durSecs
        putStrLn . printf "Found:    %d" $ objectStored
        putStrLn . printf "Speed:    %.2f files per second" $ fromIntegral objectStored / durSecs
        case I.maximum collisions of
          1 -> pure ()
          cn -> putStrLn $ "Maximum number of SHA collisions: " <> show cn
