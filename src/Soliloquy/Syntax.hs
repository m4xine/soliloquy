module Soliloquy.Syntax
  ( Pat(..)
  , PsPat 
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
import            Soliloquy.Syntax.Path       (Path)
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

type PsPat = Pat Ps 

data StringLit
  = StringLitText Text 
  deriving Show 

-- | An expression in Soliloquy.
data Expr p 
  = EVar    (Ann p) Path
  | EString (Ann p) StringLit
  | EList   (Ann p) [Expr p]
  | EInvoke (Ann p) (Expr p) (NonEmpty (Expr p))

deriving instance ShowPhrase p => Show (Expr p)

type PsExpr = Expr Ps

-- | A top-level construct in Soliloquy.  
data Toplevel p
  = TLDefVal  (Ann p) Text (Expr p) 
  | TLDefFun  (Ann p) Text [Pat p] (NonEmpty (Expr p))
  | TLDeclMod (Ann p) Text 
  | TLImport  (Ann p) Path (Maybe Path)

deriving instance ShowPhrase p => Show (Toplevel p)

type PsToplevel = Toplevel Ps