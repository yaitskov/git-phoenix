module Data.Git.Phoenix.CmdArgs where

import Data.Time.Clock
import Data.Time.Format
import Options.Applicative
import Relude
import System.FilePath ((</>))
import System.IO.Unsafe
import Data.Tagged (Tagged (..))

data InDir
data OutDir

data CmdArgs
  = CmdArgs
    { inDir :: Tagged InDir FilePath
    , outDir :: Tagged OutDir FilePath
    } deriving (Show, Eq)

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> m a
execWithArgs a = a =<< liftIO (execParser $ info (cmdp <**> helper) phelp)
  where
    cmdp = CmdArgs <$> inputDirOp <*> outputDirOp
    phelp =
      progDesc
        "git-phoenix reconstructs GIT objects after disk recovery"
        <> fullDesc

defaultOutputDir :: IO FilePath
defaultOutputDir =
  formatTime defaultTimeLocale "git-phoenix-objects-%F_%H_%M_%S"
  <$> getCurrentTime

outputDirOp :: Parser (Tagged OutDir FilePath)
outputDirOp = Tagged <$>
  strOption
  ( long "output"
    <> short 'o'
    <> showDefault
    <> value ("." </> unsafePerformIO defaultOutputDir)
    <> help ( """Path to objects folder of an uber GIT repo containing
              all discovered GIT objects. Default name is timestamp.
              Default path is current folder.""")
    <> metavar "OUTDIR"
  )

inputDirOp :: Parser (Tagged InDir FilePath)
inputDirOp = Tagged <$>
  strOption
  ( long "input"
    <> short 'i'
    <> help ( """Path to a folder with files produced by a disk recovery tool.
              e.g. photorec (testdisk). File names and locations do not matter.""")
    <> metavar "PHOTOREC-OUTPUT-DIR"
  )
