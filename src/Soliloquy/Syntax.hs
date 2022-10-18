module Soliloquy.Syntax
  ( NoAnn(..)
  , Ann(..)
  , Type(..)
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

data NoAnn = MkNoAnn deriving Show

type family Ann p :: * where
  Ann NoAnn = NoAnn
  Ann Ps    = SrcSpan
 
type ConstrainPhrase (c :: K.Type -> K.Constraint) p = (c (Ann p))

type ShowPhrase p = ConstrainPhrase Show p 

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

deriving instance ShowPhrase p => Show (Toplevel p)

type PsToplevel = Toplevel Ps