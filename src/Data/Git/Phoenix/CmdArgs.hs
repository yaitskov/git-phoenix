{-# LANGUAGE DuplicateRecordFields #-}
module Data.Git.Phoenix.CmdArgs where

import Data.Char (toLower)
import Data.Tagged (Tagged (..))
import Data.Time.Clock
import Data.Time.Format
import Options.Applicative
import Relude
import System.FilePath ((</>))
import System.IO.Unsafe
import Text.Regex.TDFA

data InDir
data OutDir
data ShaPrefix
data DaysAfter
data DaysBefore

data SearchCommitBy
  = SearchCommitBy2
    { author :: String
    , daysBefore :: Tagged DaysBefore Int
    , uberRepoDir :: Tagged InDir FilePath
    , daysAfter :: Tagged DaysAfter Int
    }
  deriving (Show, Eq)

data HeadsDiscovery
  = HeadsDiscovery2
    { author :: String
    , uberRepoDir :: Tagged InDir FilePath
    }
  deriving (Show, Eq)

data CmdArgs
  = BuildUberRepo
    { inDir :: Tagged InDir FilePath
    , outDir :: Tagged OutDir FilePath
    }
  | ExtractCommitTreeAsGitRepo
    { rootCommit :: Tagged ShaPrefix String
    , uberRepoDir :: Tagged InDir FilePath
    , gitRepoOut :: Tagged OutDir FilePath
    }
  | SearchCommitBy SearchCommitBy
  | HeadsDiscovery HeadsDiscovery
  | GitPhoenixVersion
    deriving (Show, Eq)

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> m a
execWithArgs a = a =<< liftIO (execParser $ info (cmdp <**> helper) phelp)
  where
    authorP =
      strOption (long "author" <> short 'a' <> value "." <>
                 help "Regex pattern of commit's author")
    uberP = BuildUberRepo <$> inputDirOp <*> outputDirOp
    extractP = ExtractCommitTreeAsGitRepo <$> shaP <*> inUberDirOp <*> gitOutDirOp
    headsDiscoveryP = HeadsDiscovery <$> (HeadsDiscovery2 <$> authorP <*> inUberDirOp)
    searchP =
      SearchCommitBy <$>
        (SearchCommitBy2
          <$> authorP
          <*> (Tagged <$> option auto (long "days-before" <> short 'b' <> showDefault <> value 0
                           <> help "Exclude commits older than N days"))
          <*> inUberDirOp
          <*> (Tagged <$> option auto (long "days-after" <> short 'f' <> showDefault <> value 180
                           <> help "Exclude commits newer than N days")))
    cmdp =
      hsubparser
        (  command "uber"
           (infoP uberP $
             "discovers GIT object files in disk recovery tool output and " <>
             "puts symlinks to them in a folder (uber repo)")
        <> command "extract"
           (infoP extractP "clone GIT repository with root commit sha")
        <> command "heads"
           (infoP headsDiscoveryP "discover commits without descendants")
        <> command "version"
           (infoP (pure GitPhoenixVersion) "print program version")
        <> command "search"
           (infoP searchP "find commit in the uber repo"))

    infoP p h = info p (progDesc h <> fullDesc)
    phelp =
      progDesc
        "git-phoenix reconstructs GIT repositories from output of a disk recovery tool"

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
    <> help ( "Path to objects folder of an uber GIT repo containing " <>
              "all discovered GIT objects. Default name is timestamp. " <>
              "Default path is current folder.")
    <> metavar "OUTDIR"
  )

gitOutDirOp :: Parser (Tagged OutDir FilePath)
gitOutDirOp = Tagged <$>
  strOption
  ( long "git-repo"
    <> short 'g'
    <> help "Path to output GIT repository"
    <> metavar "GIT-DIR"
  )

sha1PrefixRegex :: String
sha1PrefixRegex = "^[A-Fa-f0-9]+$"

shaP :: Parser (Tagged ShaPrefix String)
shaP = Tagged <$>
  option (maybeReader (\s -> if (s =~ sha1PrefixRegex) :: Bool
                             then Just $ fmap toLower s
                             else Nothing))
  (  long "sha-prefix"
  <> short 's'
  <> help "unique SHA1 prefix of commit tree root in hexdecimal form"
  <> metavar "SHA1"
  )

inUberDirOp :: Parser (Tagged InDir FilePath)
inUberDirOp = Tagged <$>
  strOption
  ( long "uber-dir"
    <> short 'u'
    <> help "Path to uber dir with discovered GIT objects"
    <> metavar "UBER-DIR"
  )

inputDirOp :: Parser (Tagged InDir FilePath)
inputDirOp = Tagged <$>
  strOption
  ( long "input"
    <> short 'i'
    <> help ( "Path to a folder with files produced by a disk recovery tool. " <>
              "e.g. photorec (testdisk). File names and locations do not matter.")
    <> metavar "PHOTOREC-OUTPUT-DIR"
  )
