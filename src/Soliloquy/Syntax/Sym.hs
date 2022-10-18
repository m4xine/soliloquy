module Soliloquy.Syntax.Sym
  ( Sym(..)
  , mkSym1 
  ) where

import            GHC.Show        (Show(show))
import  qualified Data.Text as T

newtype Sym = MkSym
  { unSym :: NonEmpty Text 
  } deriving (Semigroup, Eq)

instance Show Sym where
  show = T.unpack . T.intercalate "," . toList . unSym

mkSym1 :: Text -> Sym
mkSym1 = MkSym . (:| [])