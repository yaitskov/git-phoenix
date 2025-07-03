module Data.Git.Phoenix.Repo where

import Relude
import System.Directory
import System.FilePath

initGitRepo :: MonadIO m => FilePath -> m ()
initGitRepo rp = liftIO $ do
  mapM_ createDirectory $ rp : rpGit : rpGitDirs
  mapM_ (\(fn, fc) -> writeFile (rp </> fn) fc)
        [ ("HEAD", "ref: refs/heads/master")
        , ( "description"
          , "Unnamed repository; edit this file 'description' to name the repository."
          )
        , ( "config"
          , "[core]\n\trepositoryformatversion = 0\n\t" <>
            "filemode = true\n\tbare = false\n\tlogallrefupdates = true\n"
          )
        ]
  where
    rpGit = rp </> ".git"
    rpGitDirs =
      fmap (rpGit </>) $  ["branches", "hooks", "info"]
                       <> fmap ("objects" </>) ["info", "pack"]
                       <> fmap ("refs" </>) ["heads", "tags"]
