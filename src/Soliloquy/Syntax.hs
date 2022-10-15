module Soliloquy.Syntax
  ( NoAnn(..)
  , Ann(..)
  , Type(..)
  , Pat(..)
  , VarLit(..)
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

data VarLit 
  = VarLitName Text
  deriving Show

data StringLit
  = StringLitText Text 
  deriving Show 

-- | An expression in Soliloquy.
data Expr p 
  = EVar    (Ann p) VarLit
  | EString (Ann p) StringLit
  | EList   (Ann p) [Expr p]

deriving instance ShowPhrase p => Show (Expr p)

type PsExpr = Expr Ps

-- | A top-level construct in Soliloquy.  
data Toplevel p
  = SDefVal (Ann p) VarLit (Expr p) 
  | SDefFun (Ann p) VarLit [Pat p] (NonEmpty (Expr p))

deriving instance ShowPhrase p => Show (Toplevel p)

type PsToplevel = Toplevel Ps