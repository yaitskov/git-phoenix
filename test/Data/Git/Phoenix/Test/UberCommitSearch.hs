module Data.Git.Phoenix.Test.UberCommitSearch where

import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.CommitSearch
import Data.Git.Phoenix.Prelude
import Data.Git.Phoenix.Test
import Data.Time
-- import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.System
-- import Data.Time.Format
import Test.Tasty.HUnit

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0
{-# INLINE epoch #-}

utcTimeToMicros :: UTCTime -> Integer
utcTimeToMicros t = diffTimeToPicoseconds (realToFrac (diffUTCTime t epoch)) `div` 1000000

utcTimeToMillis :: UTCTime -> Integer
utcTimeToMillis = (`div` 1000) . utcTimeToMicros

utcTimeToSeconds :: UTCTime -> Integer
utcTimeToSeconds = (`div` 1000) . utcTimeToMillis

unit_uber_commit_search :: IO ()
unit_uber_commit_search = withUber go
  where
    matchingAuthor = "Daniil"
    sunny uberDir daysToLastcommit = do
      commits <- runCommitSearch $ SearchCommitBy2
        { author = matchingAuthor
        , daysBefore = Tagged $ 0 + daysToLastcommit
        , uberRepoDir = Tagged uberDir
        , daysAfter = Tagged $ 10000 + daysToLastcommit
        }
      assertEqual ("no commits matching author name") (commits == []) False
    authorMismatch uberDir daysToLastcommit = do
      commits <- runCommitSearch $ SearchCommitBy2
        { author = "^Ladiin"
        , daysBefore = Tagged $ 0 + daysToLastcommit
        , uberRepoDir = Tagged uberDir
        , daysAfter = Tagged $ 10000 + daysToLastcommit
        }
      assertEqual "author does not match" [] commits

    outOfDayRange uberDir daysToLastcommit = do
      commits <- runCommitSearch $ SearchCommitBy2
        { author = matchingAuthor
        , daysBefore = Tagged $ 20 + daysToLastcommit
        , uberRepoDir = Tagged uberDir
        , daysAfter = Tagged $ 1000 + daysToLastcommit
        }
      assertEqual ("no commits matching day range") [] commits

    go :: Tagged Root FilePath -> Tagged Uber FilePath -> IO ()
    go _rdir (Tagged uberDir) = do
      now :: Int64 <- liftIO (systemSeconds <$> getSystemTime)
      lastCommitEpoch :: Int64 <- fromIntegral . utcTimeToSeconds <$> parseTimeM False defaultTimeLocale "%Y-%m-%d" "2025-07-06"
      let daysToLastcommit :: Int = fromIntegral $ (now - lastCommitEpoch) `div` 86400
      sunny uberDir daysToLastcommit
      authorMismatch uberDir daysToLastcommit
      outOfDayRange uberDir daysToLastcommit
