module Soliloquy.Type 
  ( Type(..)
  ) where

import  Prelude               hiding  (Type)
import  Soliloquy.Syntax.Path         (Path)

data Type
  = TVar    Path
  | TString 
  | TList   Type
  | TDynamic
  deriving Show