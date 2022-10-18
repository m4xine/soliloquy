module Soliloquy.Type 
  ( Type(..)
  ) where

import  Prelude hiding (Type)

data Type
  = TString 
  | TList   Type
  | TDynamic
  deriving Show