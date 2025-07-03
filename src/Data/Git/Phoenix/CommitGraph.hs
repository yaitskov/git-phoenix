-- | Find commits reachable from target root one
module Data.Git.Phoenix.CommitGraph where

import Control.Exception (throw)
import Data.Git.Phoenix.Sha
import Data.Map.Strict qualified as M
import Relude
import UnliftIO.Exception qualified as U

data MetaCommit
  = MetaCommit
    { parent :: Maybe ComHash
    , uniqueCollisions :: !Word
    } deriving (Show, Eq)

newtype CommitGraph = CommitGraph (M.Map ComHash MetaCommit) deriving (Show, Eq)
newtype NoCommitInGraph = NoCommitInGraph ComHash deriving (Show, Eq)
instance Exception NoCommitInGraph

data ParentShaConflict
  = ParentShaConflict
    { commit :: ComHash
    , parents :: [Maybe ComHash]
    } deriving (Show)
instance Exception ParentShaConflict

empty :: CommitGraph
empty = CommitGraph mempty

addCommit :: CommitGraph -> ComHash -> Maybe ComHash -> CommitGraph
addCommit (CommitGraph g) ch pch = CommitGraph $ M.insertWith merge ch mc g
  where
    mc = MetaCommit pch 1
    merge (MetaCommit op oc) (MetaCommit np nc)
      | op == np = MetaCommit np $ oc + nc
      | otherwise = throw $ ParentShaConflict ch [op, np]
        -- error $ "Parent SHA conflict in " <> showDigest ch <> " commit: "
        --   <> maybe "n/a" showDigest op <> " <> "
        --   <> maybe "n/a" showDigest np

visitCommitChain :: MonadIO m =>
  CommitGraph ->
  ComHash ->
  (ComHash -> Word -> m (Maybe Word)) ->
  (ComHash -> Maybe Word -> m ())
  -> m ()
visitCommitChain (CommitGraph g) rootHash disambigateHash cb = go rootHash
  where
    go curHash =
      case M.lookup curHash g of
        Nothing -> U.throwIO $ NoCommitInGraph curHash
        Just (MetaCommit pc n)
          | n == 0 ->
              cb curHash Nothing >> mapM_ go pc
          | otherwise ->
              disambigateHash curHash n >>= cb curHash >> mapM_ go pc
