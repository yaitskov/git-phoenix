module Data.Git.Phoenix where

import Codec.Compression.Zlib qualified as Z
import Data.ByteString.Lazy qualified as L
import Data.Conduit (runConduitRes, (.|))
import Conduit ( MonadUnliftIO, MonadResource, ConduitT
               , filterC, foldMC, mapC, mapMC, sourceDirectoryDeep)
import Data.Digest.Pure.SHA (Digest, SHA1State, showDigest, sha1)
import Data.Git.Phoenix.CmdArgs (InDir, OutDir)
import Data.List qualified as I
import Data.Map.Strict qualified as M
import Data.Tagged (Tagged (..), untag)
-- import Hexdump (simpleHex)
import Relude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), dropFileName)

data GitObject
  = GitObject
    { gobHash :: Digest SHA1State
    , compressedObject :: L.ByteString
    }
    deriving (Show, Eq)

gitObjectFilePath :: GitObject -> FilePath
gitObjectFilePath = uncurry (</>) . I.splitAt 2 . showDigest . gobHash

newtype PhoenixConf
  = PhoenixConf
    { destObjectDir :: Tagged OutDir FilePath
    }
    deriving (Show, Eq)

type PhoenixCoCon m = (MonadUnliftIO m, MonadResource m)

mkGitObject :: L.ByteString -> GitObject
mkGitObject bs = GitObject (sha1 bs) (Z.compress bs)

findGitObjects :: PhoenixCoCon m => FilePath -> ConduitT i GitObject m ()
findGitObjects photorecOutDir =
  sourceDirectoryDeep False photorecOutDir
  .| mapMC (\fp -> liftIO (L.readFile fp))
  .| filterC zlibP
  .| mapC Z.decompress
  .| filterC gitObjectP
  .| mapC mkGitObject
  where
    zlibNoCompression = "\x0078\x0001"
    zlibDefaultCompression = "\x0078\x009C"
    zlibBestCompression = "\x0078\x00DA"

    zlibP bs =
      zlibNoCompression `L.isPrefixOf` bs ||
      zlibDefaultCompression `L.isPrefixOf` bs ||
      zlibBestCompression `L.isPrefixOf` bs

    gitObjectP bs =
        "blob " `L.isPrefixOf` bs ||
        "tree " `L.isPrefixOf` bs ||
        "commit " `L.isPrefixOf` bs

type ShaDedupMap = M.Map (Digest SHA1State) Int

alrr :: Monad m => (x -> m y) -> (x, z) -> m z
alrr f (a, r) = f a >> pure r

storeGitObject ::
  (MonadReader PhoenixConf m, PhoenixCoCon m) =>
  ShaDedupMap -> GitObject -> m ShaDedupMap
storeGitObject dedupMap gob =
  alrr writeGitObject $ M.insertLookupWithKey (\_h -> (+)) (gobHash gob) 1 dedupMap
  where
   gobPath = gitObjectFilePath gob
   writeGitObject dedupSuffix = do
     dod <- asks $ untag . destObjectDir
     liftIO $ do
       createDirectoryIfMissing False (dod </> dropFileName gobPath)
       L.writeFile (dod </> maybe gobPath (((gobPath <> ".") <>) . show) dedupSuffix)
         $ compressedObject gob

recoverFrom :: (MonadReader PhoenixConf m, MonadUnliftIO m) => Tagged InDir FilePath -> m ()
recoverFrom (Tagged photorecOutDir) = go >>= reportCollisions
  where
    go =
      M.elems <$>
        runConduitRes
          (  findGitObjects photorecOutDir
          .| foldMC storeGitObject mempty
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
