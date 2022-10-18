module Soliloquy.Syntax.Ann
  ( NoAnn(..)
  , Ann(..)
  , ConstrainPhrase
  , ShowPhrase
  ) where
    
import  qualified Data.Kind         as K
import            Soliloquy.Source        (SrcSpan)
import            Soliloquy.Pass          (Ps)

data NoAnn = MkNoAnn deriving Show

type family Ann p :: * where
  Ann NoAnn = NoAnn
  Ann Ps    = SrcSpan
 
type ConstrainPhrase (c :: K.Type -> K.Constraint) p = (c (Ann p))

type ShowPhrase p = ConstrainPhrase Show p 