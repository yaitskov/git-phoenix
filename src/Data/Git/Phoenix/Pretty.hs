{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Git.Phoenix.Pretty
  ( doc
  , hsep
  , vsep
  , (&!)
  , ($$)
  , tab
  , printDoc
  , apNe
  , module PP
  ) where

import Data.Time ( NominalDiffTime )
import Control.Exception ( IOException )
import Relude
import Text.PrettyPrint.Leijen.Text as PP hiding
  ( (<$>), bool, group, hsep, vsep, empty, isEmpty, (</>) )
import Text.PrettyPrint.Leijen.Text qualified as PP

infixr 5 $$
($$) :: Doc -> Doc -> Doc
($$) = (<$$>)

doc :: Pretty a => a -> Doc
doc = pretty

hsep :: Pretty a => [a] -> Doc
hsep = PP.hsep . fmap doc
{-# INLINE hsep #-}

vsep :: Pretty a => [a] -> Doc
vsep = vcat . fmap doc
{-# INLINE vsep #-}

printDoc :: (MonadIO m, Pretty a) => a -> m ()
printDoc x = liftIO (putDoc $ pretty x)

tab :: Pretty a => a -> Doc
tab = nest 2 . doc

class IsEmpty a where
  isEmpty :: a -> Bool

instance IsEmpty [a] where
  isEmpty = null

apNe :: (IsEmpty a, Pretty a) => a -> (Doc -> Doc) -> Doc
apNe d f
  | isEmpty d = d'
  | otherwise = f d'
  where
    d' = doc d

(&!) :: (IsEmpty a, Pretty a) => a -> (Doc -> Doc) -> Doc
(&!) = apNe

infixl 7 &!

instance Pretty IOException where
  pretty = text . show

instance Pretty a => Pretty (Set a) where
  pretty x = "{" <+> hsep (toList x) <+> "}"

instance Pretty NominalDiffTime where
  pretty = text . show
