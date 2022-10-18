module Soliloquy.Syntax.Obj
  ( ListKind(..)
  , ObjF(..)
  , SrcObj
  ) where

import  Control.Comonad.Cofree  (Cofree)
import  Text.Show.Deriving      (deriveShow1)
import  Soliloquy.Source        (SrcSpan)
import  Soliloquy.Syntax.Sym    (Sym)

-- | Kind of list object.
data ListKind
  = CurlyList   -- ^ {...}
  | BracketList -- ^ [...]
  | ParenList   -- ^ (...)
  deriving (Eq, Show) 

-- | Soliloquy Object
data ObjF a 
  = OSym    Sym
  | OString Text
  | OList   ListKind [a]

deriveShow1 ''ObjF

-- | Soliloquy object with attached source information.
type SrcObj = Cofree ObjF SrcSpan