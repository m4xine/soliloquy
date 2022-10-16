module Soliloquy.Parser.Phase2 
  ( p2
  ) where

import  Prelude                           hiding  (sym)
import  Control.Monad.Combinators                 (choice)
import  Control.Comonad.Cofree                    (Cofree((:<)))
import  Soliloquy.Obj                             (SrcObj, ListKind (..), ObjF (..))
import  Soliloquy.Parser.Phase2.Internal          (P2, runMatchList, match, chomp, parentSrc, chomps, parseError, runP2)
import  Soliloquy.Parser.Error                    (ParseError)
import  Soliloquy.Syntax                          (PsToplevel, PsExpr, Toplevel (..), VarLit(..), Expr(..), StringLit (..))

sym :: P2 Text
sym = match $ \case
  _ :< OSym s -> pure s
  _ -> parseError "Expected symbol" 

matchSym :: Text -> P2 Text
matchSym kw = 
  ifM ((kw ==) <$> sym) 
    (pure kw) 
    (parseError $ "Expected keyword '" <> kw <> "'")

string :: P2 Text
string = match $ \case
  _ :< OString s -> pure s
  _ -> parseError "Expected string"

varLit :: P2 VarLit
varLit = VarLitName <$> sym 

stringLit :: P2 StringLit
stringLit = StringLitText <$> string

exprVar :: P2 PsExpr
exprVar = EVar <$> parentSrc <*> varLit

exprString :: P2 PsExpr
exprString = EString <$> parentSrc <*> stringLit

exprList :: P2 PsExpr
exprList = 
  EList <$> parentSrc <*> runMatchList BracketList (chomps expr)

expr :: P2 PsExpr
expr = choice [ exprVar, exprString, exprList ] 

tlDefVal :: P2 PsToplevel
tlDefVal = do
  src <- parentSrc
  runMatchList ParenList $ do
    chomp $ matchSym "def"
    name <- chomp varLit
    body <- chomp expr
    pure $ TLDefVal src name body

tlDeclMod :: P2 PsToplevel
tlDeclMod = do
  src <- parentSrc
  runMatchList ParenList $ do
    chomp $ matchSym "module"
    name <- chomp varLit
    pure $ TLDeclMod src name

toplevel :: P2 PsToplevel
toplevel = choice [tlDefVal, tlDeclMod] 

-- | Parses a list of Soliloquy objects into a concise syntax representation.
p2 :: [SrcObj] -> Either [ParseError] [PsToplevel]
p2 objs = forM objs $ runP2 toplevel