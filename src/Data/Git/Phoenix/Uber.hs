-- | Collect links to GIT objects under 1 directory
module Data.Git.Phoenix.Uber where

import Codec.Compression.Zlib qualified as Z
import Control.DeepSeq
import Conduit (MonadUnliftIO, ConduitT, foldMC, mapMC, concatC, sourceDirectoryDeep)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Conduit (runConduitRes, (.|))
import Data.Git.Phoenix.App
import Data.Git.Phoenix.CmdArgs (InDir)
import Data.Git.Phoenix.Io
import Data.Git.Phoenix.Object
import Data.Git.Phoenix.Sha
import Data.List qualified as I
import Data.Map.Strict qualified as M
import Data.Tagged (Tagged (..), untag)
import Relude
import System.FilePath ((</>), dropFileName)
import UnliftIO.Exception qualified as U
import UnliftIO.Directory qualified as U
import UnliftIO.IO qualified as U

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

mkGitObject :: PhoenixM m => FilePath -> m (Maybe GitObject)
mkGitObject fp =
  withHandle fp $ \inH -> do
    magicBs <- hGet inH 2
    if zlibP magicBs
      then do
        !headerBs <- (magicBs <>) <$> hGet inH 10
        (`U.catch` skipCorruptedFile) $ do
          if gitObjectP $ Z.decompress (toLazy headerBs)
            then do
              !goh <- sha1 . Z.decompress . (toLazy headerBs <>) <$> hGetContents inH
              pure . Just $ GitObject goh fp
            else pure Nothing
      else pure Nothing
  where
    skipCorruptedFile (_ :: Z.DecompressError) = do
      putStrLn $ "Skip corrupted file: " <> fp
      pure Nothing

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
  firstGobOrigin <- L8.pack <$> U.getSymbolicLinkTarget uberGob
  U.removeFile uberGob
  U.withBinaryFile uberGob U.WriteMode $ \oh ->
    hPut oh . mconcat $ [ compressedDisambiguate
                        , B.encode $ L.length firstGobOrigin
                        , firstGobOrigin
                        , B.encode $ L.length gobPacked
                        , gobPacked
                        ]
  where
    gobPacked = L8.pack $ gobOrigin gob

appendPathToUberGob :: MonadUnliftIO m => FilePath -> GitObject -> m ()
appendPathToUberGob uberGob gob =
  U.withBinaryFile uberGob U.AppendMode $ \oh ->
    hPut oh $ gobLen <> gobPacked
  where
    gobPacked = L8.pack $ gobOrigin gob
    gobLen = B.encode $ L.length gobPacked

storeGitObject :: PhoenixUberM m => (ShaDedupMap, Int, Int) -> GitObject -> m (ShaDedupMap, Int, Int)
storeGitObject (dedupMap, !countDown, !mapSize) gob = do
  when (countDown == 0) $ do
    putStrLn $ "GIT objects found: " <> show mapSize
  -- todo: dynamic init countDown value to keep print period ~ a few seconds
  -- usb stick is slow, but SSD is fast
  (,if countDown <= 0 then 1000 else countDown - 1, mapSize + 1)
    <$> (alrr writeGitObject $ M.insertLookupWithKey (\_h -> (+)) (gobHash gob) 1 dedupMap)
  where
   gobPath = gitObjectFilePath gob
   writeGitObject = \case
     Nothing -> do
       dod <- asks $ untag . destObjectDir
       U.createDirectoryIfMissing False (dod </> dropFileName gobPath)
       U.createFileLink (gobOrigin gob) (dod </> gobPath)
     Just _dedupSuffix -> do
       dod <- asks $ untag . destObjectDir
       let uberGob = dod </> gobPath
       U.pathIsSymbolicLink (gobOrigin gob) >>= \case
         True ->
           replaceSymLinkWithDisambiguate uberGob gob
         False ->
           appendPathToUberGob uberGob gob

recoverFrom :: PhoenixUberM m => Tagged InDir FilePath -> m ()
recoverFrom (Tagged photorecOutDir) =
  U.makeAbsolute photorecOutDir >>= go >>= reportCollisions
  where
    go absInDir =
      M.elems . (\(m, _, _) -> m) <$>
        runConduitRes
          (  findGitObjects absInDir
          .| foldMC storeGitObject (mempty, 10, 0)
          )
    reportCollisions = \case
      [] ->
        putStrLn $ "Dir [" <> photorecOutDir <> "] doesn't have Git files"
      x ->
        case I.maximum x of
          1 -> pure ()
          cn -> putStrLn $ "Maximum number of SHA collisions: " <> show cn
