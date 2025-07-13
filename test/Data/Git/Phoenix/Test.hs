module Data.Git.Phoenix.Test where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.CmdRun
import Data.Git.Phoenix.Prelude
import Test.QuickCheck as QC
import UnliftIO.Directory
import UnliftIO.IO ( SeekMode(AbsoluteSeek), hSeek, withBinaryFile )
import UnliftIO.Temporary

currentHead :: String
currentHead = "62324a152b2f9272c922571ab1e08a5212da2d65"

readBranchCommit :: FilePath -> IO String
readBranchCommit fp = C8.unpack . BS.takeWhile isHexDigit <$> BS.readFile fp

data Root
data Uber

withUber :: (Tagged Root FilePath -> Tagged Uber FilePath -> IO ()) -> IO ()
withUber doWithUberDir =
  withSystemTempDirectory "gitphoenix" $ \rdir ->
    let phOut = rdir </> "photorec-output" in do
      createDirectory phOut
      forM_ cases $ \(d, f) -> do
        let destDir = phOut </> d
        createDirectory destDir
        runConduitRes
          (  sourceDirectoryDeep False "test-git-objects"
          .| foldMC (f destDir) (0 :: Int)
          )
      let uberOut = rdir </> "uber"
      runCmd BuildUberRepo { inDir = Tagged phOut
                           , outDir = Tagged uberOut
                           }
      doWithUberDir (Tagged @Root rdir) (Tagged @Uber uberOut)
  where
    cases =
      [ ("trail-trash", trailTrash)
      , ("middle-trash", middleTrash)
      , ("symlinks", symlinks)
      , ("clean-copy", cleanCopy)
      ]
    markWritable fp =
      setPermissions fp . setOwnerWritable True =<< getPermissions fp
    trailTrash destDir nextFileName gitObjFp = liftIO $ do
      let destFp = destDir </> show nextFileName <> ".go1"
      copyFile gitObjFp destFp
      markWritable destFp
      withBinaryFile destFp AppendMode $ \h -> genBs >>= BS.hPut h
      pure $ nextFileName + 1
    middleTrash destDir nextFileName gitObjFp = liftIO $ do
      let destFp = destDir </> show nextFileName <> ".go1"
      copyFile gitObjFp destFp
      markWritable destFp
      withBinaryFile destFp WriteMode $ \h -> do
        hs <- fromIntegral <$> getFileSize gitObjFp
        i <- fromIntegral <$> generate (chooseInt (0, hs - 1))
        hSeek h AbsoluteSeek i
        genBs >>= BS.hPut h
      pure $ nextFileName + 1
    symlinks destDir nextFileName gitObjFp = liftIO $ do
      let destFp = destDir </> show nextFileName <> ".go1"
      (`createFileLink` destFp) =<< makeAbsolute gitObjFp
      pure $ nextFileName + 1
    cleanCopy destDir nextFileName gitObjFp = liftIO $ do
      let destFp = destDir </> show nextFileName <> ".go1"
      copyFile gitObjFp destFp
      pure $ nextFileName + 1

genBs :: MonadIO m => m ByteString
genBs = liftIO $ generate go
  where
    go = do
      s <- QC.getSize
      BS.pack <$> QC.vector (1 + s)
