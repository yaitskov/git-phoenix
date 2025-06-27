module Data.Git.Phoenix where

import Codec.Compression.Zlib qualified as Z
import Conduit ( MonadUnliftIO, MonadResource, ConduitT
               , foldMC, mapMC, concatC, sourceDirectoryDeep)
import Data.ByteString.Lazy qualified as L
import Data.ByteString qualified as BS
import Data.Conduit (runConduitRes, (.|))
import Data.Digest.Pure.SHA (Digest, SHA1State, showDigest, sha1)
import Data.Git.Phoenix.CmdArgs (InDir, OutDir)
import Data.List qualified as I
import Data.Map.Strict qualified as M
import Data.Tagged (Tagged (..), untag)
-- import Hexdump (simpleHex)
import Relude
import System.FilePath ((</>), dropFileName)
import UnliftIO.Exception qualified as U
import UnliftIO.QSem qualified as U
import UnliftIO.IO qualified as U
import UnliftIO.Directory qualified as U

data GitObject
  = GitObject
    { gobHash :: !(Digest SHA1State)
    , gobOrigin :: !FilePath
    }
    deriving (Show, Eq)

gitObjectFilePath :: GitObject -> FilePath
gitObjectFilePath = uncurry (</>) . I.splitAt 2 . showDigest . gobHash

data PhoenixConf
  = PhoenixConf
    { destObjectDir :: Tagged OutDir FilePath
    , inHandlesSem  :: U.QSem
    }

type PhoenixM m =
  ( MonadUnliftIO m
  , MonadReader PhoenixConf m
  )

type PhoenixCoCon m = (PhoenixM m, MonadResource m)

mkGitObject :: PhoenixM m => FilePath -> m (Maybe GitObject)
mkGitObject fp = do
  s <- asks inHandlesSem
  U.bracket_ (U.waitQSem s) (U.signalQSem s) $
    U.withBinaryFile fp U.ReadMode $ \inH -> liftIO $ do
     magicBs <- BS.hGet inH 2
     if zlibP magicBs
       then do
         !headerBs <- (magicBs <>) <$> BS.hGet inH 32
         if gitObjectP $ Z.decompress (toLazy headerBs)
           then do
             !goh <- sha1 . Z.decompress . (toLazy headerBs <>) <$> L.hGetContents inH
             pure . Just $ GitObject goh fp
           else pure Nothing
       else pure Nothing
  where
    zlibNoCompression = "\x0078\x0001"
    zlibDefaultCompression = "\x0078\x009C"
    zlibBestCompression = "\x0078\x00DA"

    zlibP bs =
      zlibNoCompression == bs ||
      zlibDefaultCompression == bs ||
      zlibBestCompression == bs

    gitObjectP bs =
        "blob " `L.isPrefixOf` bs ||
        "tree " `L.isPrefixOf` bs ||
        "commit " `L.isPrefixOf` bs

findGitObjects :: PhoenixCoCon m => FilePath -> ConduitT i GitObject m ()
findGitObjects photorecOutDir =
  sourceDirectoryDeep False photorecOutDir
  .| mapMC mkGitObject
  .| concatC

type ShaDedupMap = M.Map (Digest SHA1State) Int

alrr :: Monad m => (x -> m y) -> (x, z) -> m z
alrr f (a, !r) = f a >> pure r

storeGitObject :: PhoenixCoCon m => (ShaDedupMap, Int, Int) -> GitObject -> m (ShaDedupMap, Int, Int)
storeGitObject (dedupMap, !countDown, !mapSize) gob = do
  when (countDown == 0) $ do
    putStrLn $ "GIT objects found: " <> show mapSize
  (,if countDown <= 0 then 1000 else countDown - 1, mapSize + 1)
    <$> (alrr writeGitObject $ M.insertLookupWithKey (\_h -> (+)) (gobHash gob) 1 dedupMap)
  where
   gobPath = gitObjectFilePath gob
   writeGitObject dedupSuffix = do
     dod <- asks $ untag . destObjectDir
     U.createDirectoryIfMissing False (dod </> dropFileName gobPath)
     U.createFileLink
       (gobOrigin gob)
       (dod </> maybe gobPath (((gobPath <> ".") <>) . show) dedupSuffix)

recoverFrom :: PhoenixM m => Tagged InDir FilePath -> m ()
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

-- blobPath :: FilePath
-- blobPath = "/home/dan/pro/git-phoenix/.git/objects/04/327561a0005dc9ac2742e88ff4d059bf122ca2"
-- foo :: IO ()
-- foo = do
--   print . showDigest . sha1 . Z.decompress . (<> "aoeu") =<< L.readFile blobPath
--   print . L.take 42 . Z.decompress . L.take 48 =<< L.readFile blobPath
--   print $ simpleHex $ toStrict $ Z.compressWith
--     (Z.defaultCompressParams { Z.compressLevel = Z.CompressionLevel 0 })
--     "Hello World"
--   print . Z.decompress . (<> "aoeu") =<< L.readFile blobPath
--   print . simpleHex . toStrict =<< L.readFile blobPath
--   print $ simpleHex $ toStrict $ Z.compress "Hello World"
--   print . Z.decompress $ Z.compress "Hello World"
