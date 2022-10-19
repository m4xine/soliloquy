module Soliloquy.Syntax.Path
  ( Path(..)
  ) where

import            GHC.Show        (Show(show))
import  qualified Data.Text as T

-- | Symbol representation.
newtype Path = MkPath
  { unPath :: NonEmpty Text 
  } deriving (Semigroup, Eq)

instance Show Path where
  show = T.unpack . T.intercalate "." . toList . unPath