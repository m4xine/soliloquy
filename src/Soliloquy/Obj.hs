module Soliloquy.Obj
  ( ListKind(..)
  , ObjF(..)
  , SrcObj
  ) where

import  Control.Comonad.Cofree  (Cofree)
import  Text.Show.Deriving      (deriveShow1)
import  Soliloquy.Source        (SrcSpan)

-- | Kind of list object.
data ListKind
  = CurlyList   -- ^ {...}
  | BracketList -- ^ [...]
  | ParenList   -- ^ (...)
  deriving Show 

-- | Soliloquy Object
data ObjF a 
  = OVar    Text
  | OString Text
  | OList   ListKind [a]

deriveShow1 ''ObjF

-- | Soliloquy object with attached source information.
type SrcObj = Cofree ObjF SrcSpan