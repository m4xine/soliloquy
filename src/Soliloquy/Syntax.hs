module Soliloquy.Syntax
  ( Type(..)
  , Pat(..)
  , StringLit(..)
  , Expr(..)
  , PsExpr
  , Toplevel(..)
  , PsToplevel
  ) where

import            Prelude           hiding    (Type)
import  qualified Data.Kind             as K
import            Soliloquy.Source            (SrcSpan)
import            Soliloquy.Pass              (Ps)
import            Soliloquy.Type              (Type(..))
import            Soliloquy.Syntax.Sym        (Sym)
import            Soliloquy.Syntax.Ann        (Ann, ShowPhrase)

-- | A pattern in Soliloquy.
data Pat p 
  = PVar 
      { pvarText  :: Text
      , pvarAnn   :: Ann p
      }
  | PSig 
      { psigPat :: Pat p
      , psigSig :: Type
      , psigAnn :: Ann p
      } 

deriving instance ShowPhrase p => Show (Pat p)

data StringLit
  = StringLitText Text 
  deriving Show 

-- | An expression in Soliloquy.
data Expr p 
  = EVar    (Ann p) Sym
  | EString (Ann p) StringLit
  | EList   (Ann p) [Expr p]

deriving instance ShowPhrase p => Show (Expr p)

type PsExpr = Expr Ps

-- | A top-level construct in Soliloquy.  
data Toplevel p
  = TLDefVal  (Ann p) Sym (Expr p) 
  | TLDefFun  (Ann p) Sym [Pat p] (NonEmpty (Expr p))
  | TLDeclMod (Ann p) Sym 
  | TLImport  (Ann p) Sym (Maybe Sym)

deriving instance ShowPhrase p => Show (Toplevel p)

type PsToplevel = Toplevel Ps