module RunUberExtract where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.CmdRun
import Data.Git.Phoenix.Prelude
import Test.QuickCheck as QC
import UnliftIO.Directory
-- ( getPermissions, setPermissions
--                           , setOwnerWritable, getFileSize
--                           , removePathForcibly, makeAbsolute
--                           )
import UnliftIO.IO (hSeek, SeekMode (..))
import UnliftIO.Process
import UnliftIO.Temporary

main :: IO ()
main = -- do
    -- let rdir = "tmpdir"
    -- removePathForcibly rdir
    -- createDirectory rdir
  withSystemTempDirectory "gitphoenix" $ \rdir ->
    let phOut = rdir </> "photorec-output" in do
      createDirectory phOut
      forM_ cases $ \(d, f) -> do
        let destDir = phOut </> d
        createDirectory destDir
        runConduitRes
          (  sourceDirectoryDeep False (".git" </> "objects")
          .| foldMC (f destDir) (0 :: Int)
          )
      let uberOut = rdir </> "uber"
      runCmd BuildUberRepo { inDir = Tagged phOut
                           , outDir = Tagged uberOut
                           }
      currentHead <- readBranchCommit ".git/refs/heads/master"
      let gitOut = rdir </> "git-phoenix"
      -- putStrLn $ "gitOut " <> show gitOut <> "; com " <> currentHead <> " ; uberOut " <> uberOut
      -- eeeetHead <- BS.readFile ".git/objects/69/6b0b2d3b87e8fa4532a0e0da69baadaa8dd42a"
      -- putStrLn $ "[" <> show (C8.unpack currentHead) <> "] show eeeetHead

      -- putStrLn =<< readProcess "ls" ["-l", uberOut </> "69"] "" -- callCommand "ls -l ; pwd"
      -- putStrLn =<< readProcess "ls" ["-l", uberOut] "" -- callCommand "ls -l ; pwd"

      runCmd ExtractCommitTreeAsGitRepo { rootCommit = Tagged currentHead
                                        , uberRepoDir = Tagged uberOut
                                        , gitRepoOut = Tagged gitOut
                                        }
      callCommand $ "git -C " <> gitOut <> " fsck --full"
      gotHead <- readBranchCommit (gitOut </> ".git/refs/heads/master")
      when (gotHead /= currentHead) $ do
        fail $ "HEAD commit mismatch\nGot:      [" <> gotHead
                               <> "]\nExpected: [" <> currentHead <> "]"
  where
    readBranchCommit fp = C8.unpack . BS.takeWhile isHexDigit <$> BS.readFile fp
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

  {-
     creat temp dir as photorec output
     inside dir that dir
       find files from .git/objects and symlink them
       put links in 2 dirs - for duplicate simulation
       copy with trash bytes - for duplicate simulation
     run Uber
     run Extract
     run git fsck --full
      $(git log -1 | grep -o -E 'commit [a-f0-9]+'
         $(git status | wc -l) == 0
   -}
