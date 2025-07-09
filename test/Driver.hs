module Driver where

import qualified Discovery
import Relude
import Test.Tasty

main :: IO ()
main = defaultMain =<< testTree
  where
    testTree :: IO TestTree
    testTree = do
      tests <- Discovery.tests
      pure $ testGroup "git-phoenix" [ tests ]
