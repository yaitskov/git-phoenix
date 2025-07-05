module Main where

import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.CmdRun
import Data.Git.Phoenix.Prelude

main :: IO ()
main = execWithArgs runCmd
