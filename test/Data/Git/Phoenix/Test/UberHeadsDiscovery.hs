module Data.Git.Phoenix.Test.UberHeadsDiscovery where

import Data.ByteString.Lazy qualified as L
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.HeadsDiscovery
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Test
import Test.Tasty.HUnit

unit_format_commit :: IO ()
unit_format_commit =
  assertEqual
    "gold"
    ("00010203 2025-07-06 15:12 Daniil Iaitskov  Hello\nWorld\n" :: String)
    (show $ commitObjectsToDoc
     [ ( L.pack [0..19]
       , CommitObject
         { message = "Hello\nWorld\n"
         , parent = Nothing
         , commitTs = 1751814758
         , comAuthor = "Daniil Iaitskov "
         }
       )
     ])

unit_uber_heads_discovery :: IO ()
unit_uber_heads_discovery = withUber go
  where
    go :: Tagged Root FilePath -> Tagged Uber FilePath -> IO ()
    go _rdir (Tagged uberDir) = do
      sunny uberDir
      authorMismatch uberDir

    sunny uberDir = do
      commits <- runHeadsDiscovery HeadsDiscovery2
        { author = "Daniil"
        , uberRepoDir = Tagged uberDir
        }
      assertEqual "no commits matching author name" (commits == []) False

    authorMismatch uberDir = do
      commits <- runHeadsDiscovery HeadsDiscovery2
        { author = "^Ladiin"
        , uberRepoDir = Tagged uberDir
        }
      assertEqual "author does not match" [] commits
