-- {-# LANGUAGE UndecidableInstances #-}
module Data.Git.Phoenix.App where

import Conduit (MonadUnliftIO, MonadResource)
import Data.Git.Phoenix.CmdArgs
import Data.Git.Phoenix.Io
import Data.Tagged (Tagged)
import Relude
import UnliftIO.QSem qualified as U

type PhoenixM m = (HasCallStack, MonadUnliftIO m, MonadFail m, HasInHandlesSem m)

data PhoenixUberConf
  = PhoenixUberConf
    { destObjectDir :: Tagged OutDir FilePath
    , inHandlesSem  :: U.QSem
    }

instance HasInHandlesSem (ReaderT PhoenixUberConf IO) where
  getInHandlesSem = asks inHandlesSem

type PhoenixUberM m = (PhoenixM m, MonadReader PhoenixUberConf m)

data PhoenixExtractConf
  = PhoenixExtractConf
    { destGitDir :: Tagged OutDir FilePath
    , uberDir :: Tagged InDir FilePath
    , inHandlesSem' :: U.QSem
    }

type PhoenixExtractM m = (PhoenixM m, MonadReader PhoenixExtractConf m)

instance HasInHandlesSem (ReaderT PhoenixExtractConf IO) where
  getInHandlesSem = asks inHandlesSem'

data PhoenixSearchConf
  = PhoenixSearchConf
    { uberRepoDir :: Tagged InDir FilePath
    , inHandlesSem'' :: U.QSem
    }

type PhoenixSearchM m = (PhoenixM m, MonadReader PhoenixSearchConf m)

instance HasInHandlesSem (ReaderT PhoenixSearchConf IO) where
  getInHandlesSem = asks inHandlesSem''

type PhoenixCoCon m = (PhoenixM m, MonadResource m)
